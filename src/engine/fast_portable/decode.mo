import Types "../../types";
import Slice "../../slice";
import Result "mo:base/Result";
import Nat8 "mo:base/Nat8";
import Nat64 "mo:base/Nat64";
import Array "mo:base/Array";
import Iter "mo:base/Iter";
import Prelude "mo:base/Prelude";

module {
    type DecodeError = Types.DecodeError;

    public let INVALID_VALUE: Nat8 = 255;
    public let PAD_BYTE: Nat8 = 61; // b'='

    // decode logic operates on chunks of 8 input bytes without padding
    public let INPUT_CHUNK_LEN: Nat = 8;
    public let DECODED_CHUNK_LEN: Nat = 6;

    // we read a u64 and write a u64, but a u64 of input only yields 6 bytes of output, so the last
    // 2 bytes of any output u64 should not be counted as written to (but must be available in a
    // slice).
    public let DECODED_CHUNK_SUFFIX: Nat = 2;

    // how many u64's of input to handle at a time
    public let CHUNKS_PER_FAST_LOOP_BLOCK: Nat = 4;

    public let INPUT_BLOCK_LEN: Nat = 32; // CHUNKS_PER_FAST_LOOP_BLOCK * INPUT_CHUNK_LEN;

    // includes the trailing 2 bytes for the final u64 write
    public let DECODED_BLOCK_LEN: Nat = 26; // CHUNKS_PER_FAST_LOOP_BLOCK * DECODED_CHUNK_LEN + DECODED_CHUNK_SUFFIX;

    public func write_u64(slice: Slice.Slice<Nat8>, value: Nat64) {
        let res: [var Nat8] = Array.init<Nat8>(8, 0);
        var index: Nat = 0;
        while (index < 8) {
            let temp1: Nat64 = value << (Nat64.fromNat(index) * 8);
            let temp2: Nat64 = temp1 >> ((7 - Nat64.fromNat(index)) * 8);
            res[index] := Nat8.fromNat(Nat64.toNat(temp2));
            index += 1;
        };
        slice.copy_from_slice(res);
    };
    /// Decode 8 bytes of input into 6 bytes of output. 8 bytes of output will be written, but only the
    /// first 6 of those contain meaningful data.
    ///
    /// `input` is the bytes to decode, of which the first 8 bytes will be processed.
    /// `index_at_start_of_input` is the offset in the overall input (used for reporting errors
    /// accurately)
    /// `decode_table` is the lookup table for the particular base64 alphabet.
    /// `output` will have its first 8 bytes overwritten, of which only the first 6 are valid decoded
    /// data.
    func decode_chunk(input: [Nat8], index_at_start_of_input: Nat, decode_table: [Nat8], output: Slice.Slice<Nat8>) : Result.Result<(), DecodeError> {
        var accum: Nat64 = 0;

        let morsel0 = decode_table[Nat8.toNat(input[0])];
        if (morsel0 == INVALID_VALUE) {
            return #err(#InvalidByte(index_at_start_of_input, input[0]));
        };
        accum := Nat64.fromNat(Nat8.toNat(morsel0)) << 58;

        var i: Nat = 1;
        while (i < 8) {
            let morsel = decode_table[Nat8.toNat(input[i])];
            if (morsel == INVALID_VALUE) {
                return #err(#InvalidByte(index_at_start_of_input + i, input[i]));
            };
            accum |= Nat64.fromNat(Nat8.toNat(morsel)) << (52-6*(Nat64.fromNat(i)-1));
            i += 1;
        };
        write_u64(output, accum);
        #ok(())
    };

    /// Decode an 8-byte chunk, but only write the 6 bytes actually decoded instead of including 2
    /// trailing garbage bytes.
    func decode_chunk_precise(input: [Nat8], index_at_start_of_input: Nat, decode_table: [Nat8], output: Slice.Slice<Nat8>) : Result.Result<(), DecodeError> {
        let tmp_buf = Slice.Slice<Nat8>();
        switch (decode_chunk(input, index_at_start_of_input, decode_table, tmp_buf)) {
            case (#err(e)) { return #err(e); };
            case (#ok(_)) { };
        };
        tmp_buf.sub(0,5);
        output.copy_from_slice(tmp_buf.get_mut_array());
        #ok(())
    };

    /// Return the number of input chunks (including a possibly partial final chunk) in the input
    public func num_chunks(input_len: Nat) : Nat {
        (input_len + (INPUT_CHUNK_LEN - 1)) / INPUT_CHUNK_LEN
    };

    public class FastPortableEstimate(input_len: Nat) {
        public let num_chunks_: Nat = num_chunks(input_len);

        public func decoded_length_estimate() : Nat {
            num_chunks_ * DECODED_CHUNK_LEN
        };
    };

    /// Helper to avoid duplicating num_chunks calculation, which is costly on short inputs.
    /// Returns the number of bytes written, or an error.
    // We're on the fragile edge of compiler heuristics here. If this is not inlined, slow. If this is
    // inlined(always), a different slow. plain ol' inline makes the benchmarks happiest at the moment,
    // but this is fragile and the best setting changes with only minor code modifications.
    public func decode_helper(input: [Nat8], estimate: FastPortableEstimate, output: Slice.Slice<Nat8>, decode_table: [Nat8], decode_allow_trailing_bits: Bool) : Result.Result<Nat, DecodeError> {
        let input_len = input.size();
        let remainder_len = input_len % INPUT_CHUNK_LEN;

        // Because the fast decode loop writes in groups of 8 bytes (unrolled to
        // CHUNKS_PER_FAST_LOOP_BLOCK times 8 bytes, where possible) and outputs 8 bytes at a time (of
        // which only 6 are valid data), we need to be sure that we stop using the fast decode loop
        // soon enough that there will always be 2 more bytes of valid data written after that loop.
        let trailing_bytes_to_skip = switch remainder_len {
            // if input is a multiple of the chunk size, ignore the last chunk as it may have padding,
            // and the fast decode logic cannot handle padding            
            case (0) { INPUT_CHUNK_LEN };
            // 1 and 5 trailing bytes are illegal: can't decode 6 bits of input into a byte
            case (1 or 5) {
                // trailing whitespace is so common that it's worth it to check the last byte to
                // possibly return a better error message
                let b = input[input_len-1];
                if (b != PAD_BYTE and decode_table[Nat8.toNat(b)] == INVALID_VALUE) {
                    return #err(#InvalidByte(input_len-1, b));
                };
                return #err(#InvalidLength);
            };
            // This will decode to one output byte, which isn't enough to overwrite the 2 extra bytes
            // written by the fast decode loop. So, we have to ignore both these 2 bytes and the
            // previous chunk.            
            case (2) { INPUT_CHUNK_LEN + 2 };
            // If this is 3 unpadded chars, then it would actually decode to 2 bytes. However, if this
            // is an erroneous 2 chars + 1 pad char that would decode to 1 byte, then it should fail
            // with an error, not panic from going past the bounds of the output slice, so we let it
            // use stage 3 + 4.   
            case (3) { INPUT_BLOCK_LEN + 3 };
            // This can also decode to one output byte because it may be 2 input chars + 2 padding
            // chars, which would decode to 1 byte.            
            case (4) { INPUT_BLOCK_LEN + 4 };
            // Everything else is a legal decode len (given that we don't require padding), and will
            // decode to at least 2 bytes of output. 
            case (_) { remainder_len };
        };
        // rounded up to include partial chunks
        var remaining_chunks = estimate.num_chunks_;
        
        var input_index = 0;
        var output_index = 0;

        do {
            let length_of_fast_decode_chunks = Types.saturating_sub(input.size(), trailing_bytes_to_skip);

            // Fast loop, stage 1
            // manual unroll to CHUNKS_PER_FAST_LOOP_BLOCK of u64s to amortize slice bounds checks
            switch (Types.checked_sub(length_of_fast_decode_chunks, INPUT_BLOCK_LEN)) {
                case (?max_start_index) {
                    while (input_index <= max_start_index) {
                        let output_slice = output;
                        output_slice.sub(output_index, output_index+DECODED_BLOCK_LEN-1);

                        ignore decode_chunk(input, input_index, decode_table, output_slice);
                        
                        let temp = Slice.Slice<Nat8>();
                        ignore decode_chunk(Array.freeze(Types.sub_array<Nat8>(input, 8, input.size()-1)), input_index + 8, decode_table, temp);
                        output_slice.modify_mut(6, temp.get_mut_array());

                        ignore decode_chunk(Array.freeze(Types.sub_array<Nat8>(input, 16, input.size()-1)), input_index + 16, decode_table, temp);
                        output_slice.modify_mut(12, temp.get_mut_array());

                        ignore decode_chunk(Array.freeze(Types.sub_array<Nat8>(input, 24, input.size()-1)), input_index + 24, decode_table, temp);
                        output_slice.modify_mut(18, temp.get_mut_array());

                        output.modify_mut(output_index, output_slice.get_mut_array());

                        input_index += INPUT_BLOCK_LEN;
                        output_index += DECODED_BLOCK_LEN - DECODED_CHUNK_SUFFIX;
                        remaining_chunks -= CHUNKS_PER_FAST_LOOP_BLOCK;
                    };
                };
                case (_) { };
            };

            // Fast loop, stage 2 (aka still pretty fast loop)
            // 8 bytes at a time for whatever we didn't do in stage 1.
            switch (Types.checked_sub(length_of_fast_decode_chunks, INPUT_CHUNK_LEN)) {
                case (?max_start_index) {
                    while (input_index < max_start_index) {
                        let input_slice = Slice.Slice<Nat8>();
                        let output_slice = output;
                        input_slice.copy_from_slice(Array.thaw(input));
                        input_slice.sub(input_index, input_index+INPUT_BLOCK_LEN-1);
                        output_slice.sub(output_index, output_index+DECODED_CHUNK_LEN+DECODED_CHUNK_SUFFIX-1);

                        ignore decode_chunk(input_slice.get_array(), input_index, decode_table, output_slice);
                        output.modify_mut(output_index, output_slice.get_mut_array());

                        output_index += DECODED_CHUNK_LEN;
                        input_index += INPUT_CHUNK_LEN;
                        remaining_chunks -= 1;
                    };
                };
                case (_) { };
            };
        };

        // Stage 3
        // If input length was such that a chunk had to be deferred until after the fast loop
        // because decoding it would have produced 2 trailing bytes that wouldn't then be
        // overwritten, we decode that chunk here. This way is slower but doesn't write the 2
        // trailing bytes.
        // However, we still need to avoid the last chunk (partial or complete) because it could
        // have padding, so we always do 1 fewer to avoid the last chunk.     
        for (_ in Iter.range(1, remaining_chunks-1)) {
            let input_slice = Slice.Slice<Nat8>();
            let output_slice = output;
            input_slice.copy_from_slice(Array.thaw(input));
            input_slice.sub(input_index, input.size()-1);
            output_slice.sub(output_index, output_index+DECODED_CHUNK_LEN-1);

            ignore decode_chunk_precise(input_slice.get_array(), input_index, decode_table, output_slice);
            input_index += INPUT_CHUNK_LEN;
            output_index += DECODED_CHUNK_LEN;
        };

        // Stage 4
        // Finally, decode any leftovers that aren't a complete input block of 8 bytes.
        // Use a u64 as a stack-resident 8 byte buffer.
        var leftover_bits: Nat64 = 0;
        var morsels_in_leftover: Nat64 = 0;
        var padding_bytes = 0;
        var first_padding_index: Nat = 0;
        var last_symbol = 0:Nat8;
        let start_of_leftovers = input_index;
        
        label f for (i in Iter.range(start_of_leftovers, input.size())) {
            // '=' padding
            if (input[i] == PAD_BYTE) {
                // There can be bad padding in a few ways:
                // 1 - Padding with non-padding characters after it
                // 2 - Padding after zero or one non-padding characters before it
                //     in the current quad.
                // 3 - More than two characters of padding. If 3 or 4 padding chars
                //     are in the same quad, that implies it will be caught by #2.
                //     If it spreads from one quad to another, it will be an invalid byte
                //     in the first quad.
                if (i % 4 < 2) {
                    // Check for case #2.
                    let tmp = if (padding_bytes > 0) {
                            // If we've already seen padding, report the first padding index.
                            // This is to be consistent with the faster logic above: it will report an
                            // error on the first padding character (since it doesn't expect to see
                            // anything but actual encoded data).
                            first_padding_index
                        } else {
                            // haven't seen padding before, just use where we are now
                            i
                        };
                    let bad_padding_index = start_of_leftovers + tmp;

                    return #err(#InvalidByte(bad_padding_index, input[i]));
                };
                if (padding_bytes == 0) {
                    first_padding_index := i;
                };
                padding_bytes += 1;
                continue f;
            };

            // Check for case #1.
            // To make '=' handling consistent with the main loop, don't allow
            // non-suffix '=' in trailing chunk either. Report error as first
            // erroneous padding.   

            if (padding_bytes > 0) {
                return #err(#InvalidByte(start_of_leftovers + first_padding_index, PAD_BYTE));
            };
            last_symbol := input[i];

            // can use up to 8 * 6 = 48 bits of the u64, if last chunk has no padding.
            // Pack the leftovers from left to right.            
            let shift: Nat64 = 64 - (morsels_in_leftover + 1) * 6;
            let morsel = decode_table[Nat8.toNat(input[i])];
            if (morsel == INVALID_VALUE) {
                return #err(#InvalidByte(start_of_leftovers + i, input[i]));
            };
            leftover_bits |= (Nat64.fromNat(Nat8.toNat(morsel))) << shift;
            morsels_in_leftover += 1;
        };

        // When encoding 1 trailing byte (e.g. 0xFF), 2 base64 bytes ("/w") are needed.
        // / is the symbol for 63 (0x3F, bottom 6 bits all set) and w is 48 (0x30, top 2 bits
        // of bottom 6 bits set).
        // When decoding two symbols back to one trailing byte, any final symbol higher than
        // w would still decode to the original byte because we only care about the top two
        // bits in the bottom 6, but would be a non-canonical encoding. So, we calculate a
        // mask based on how many bits are used for just the canonical encoding, and optionally
        // error if any other bits are set. In the example of one encoded byte -> 2 symbols,
        // 2 symbols can technically encode 12 bits, but the last 4 are non canonical, and
        // useless since there are no more symbols to provide the necessary 4 additional bits
        // to finish the second original byte.
        let leftover_bits_ready_to_append : Nat64 = switch (morsels_in_leftover) {
            case (0) { 0 };
            case (2) { 8 };
            case (3) { 16 };
            case (4) { 24 };
            case (6) { 32 };
            case (7) { 40 };
            case (8) { 48 };
            case (_) { Prelude.unreachable() };
        };

        // // if there are bits set outside the bits we care about, last symbol encodes trailing bits that
        // // will not be included in the output
        // let mask = Nat64.fromNat(Int.abs(-1 >> leftover_bits_ready_to_append));
        // if ((not decode_allow_trailing_bits) and (leftover_bits & mask) != 0) {
        //     // last morsel is at `morsels_in_leftover` - 1
        //     return #err(#InvalidLastSymbol(start_of_leftovers + morsels_in_leftover - 1, last_symbol));
        // };

        // TODO benchmark simply converting to big endian bytes
        var leftover_bits_appended_to_buf: Nat64 = 0;
        while (leftover_bits_appended_to_buf < leftover_bits_ready_to_append) {
            // `as` simply truncates the higher bits, which is what we want here
            let selected_bits = Nat8.fromNat(Nat64.toNat((leftover_bits >> (56 - leftover_bits_appended_to_buf))));
            output.slice[output_index] := selected_bits;
            output_index += 1;

            leftover_bits_appended_to_buf += 8;
        };

        #ok(output_index)
    };
};
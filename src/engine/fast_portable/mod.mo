import Alphabet "../../alphabet";
import Decode "./decode";
import Slice "../../slice";
import Types "../../types";
import Array "mo:base/Array";
import Nat8 "mo:base/Nat8";
import Result "mo:base/Result";
import Nat64 "mo:base/Nat64";

module {
    type DecodeError = Types.DecodeError;
    type DecodeEstimate = Decode.FastPortableEstimate;

    public let INVALID_VALUE: Nat8 = 255;

    public func read_u64(s: [Nat8]) : Nat64 {
        var index: Nat = 0;
        var val: Nat64 = 0;
        while (index < 8) {
            val += (Nat64.fromNat(Nat8.toNat(s[index])) << ((7-Nat64.fromNat(index))*8));
            index += 1;
        };
        val
    };

    /// Returns a table mapping a 6-bit index to the ASCII byte encoding of the index
    public func encode_table(alphabet: Alphabet.Alphabet) : [Nat8] {
        let encode_table: [var Nat8] = Array.init<Nat8>(64, 0);
        var index = 0;
        while (index < 64) {
            encode_table[index] := alphabet.symbols[index];
            index += 1;
        };
        Array.freeze(encode_table)
    };

    /// Returns a table mapping base64 bytes as the lookup index to either:
    /// - [INVALID_VALUE] for bytes that aren't members of the alphabet
    /// - a byte whose lower 6 bits are the value that was encoded into the index byte
    public func decode_table(alphabet: Alphabet.Alphabet) : [Nat8] {
        let decode_table: [var Nat8] = Array.init<Nat8>(256, INVALID_VALUE);
        var index = 0;
        while (index < 64) {
            decode_table[Nat8.toNat(alphabet.symbols[index])] := Nat8.fromNat(index);
            index += 1;
        };
        Array.freeze(decode_table)
    };

    public class FastPortableConfig() {
        public var encode_padding_: Bool = true;
        public var decode_allow_trailing_bits: Bool = false;

        public func with_encode_padding(padding: Bool) {
            encode_padding_ := padding;
        };

        public func with_decode_allow_trailing_bits(allow: Bool) {
            decode_allow_trailing_bits := allow;
        };

        public func encode_padding() : Bool {
            encode_padding_
        };
    };

    public func PAD() : FastPortableConfig = FastPortableConfig();
    public func NO_PAD(): FastPortableConfig {
        let f = FastPortableConfig();
        f.with_encode_padding(false);
        f
    };


    /// A general-purpose base64 engine.
    /// - It uses no vector CPU instructions, so it will work on any system.
    /// - It is reasonably fast (~2GiB/s).
    /// - It is not constant-time, though, so it is vulnerable to timing side-channel attacks. For loading cryptographic keys, etc, it is suggested to use the forthcoming constant-time implementation.
    ///
    /// Create a `FastPortable` engine from an [Alphabet].
    /// While not very expensive to initialize, ideally these should be cached
    /// if the engine will be used repeatedly.
    public class FastPortable(alphabet: Alphabet.Alphabet, _config: FastPortableConfig) {
        var encode_table_: [Nat8] = encode_table(alphabet); // len = 64
        var decode_table_: [Nat8] = decode_table(alphabet); // len = 256
        var config_: FastPortableConfig = _config;

        type Config = FastPortableConfig;

        public func encode(input: [Nat8], output: Slice.Slice<Nat8>) : Nat {
            var input_index: Nat = 0;
            
            let BLOCKS_PER_FAST_LOOP: Nat = 4;
            let LOW_SIX_BITS: Nat64 = 0x3F;

            // we read 8 bytes at a time (u64) but only actually consume 6 of those bytes. Thus, we need
            // 2 trailing bytes to be available to read..
            let last_fast_index = Types.saturating_sub(input.size(), BLOCKS_PER_FAST_LOOP * 6 + 2);
            var output_index = 0;

            if (last_fast_index > 0) {
                while (input_index <= last_fast_index) {
                    // Major performance wins from letting the optimizer do the bounds check once, mostly
                    // on the output side
                    let input_chunk = Array.freeze(Types.sub_array<Nat8>(input, input_index, input_index + (BLOCKS_PER_FAST_LOOP*6+2)));
                    let output_chunk = output;
                    output_chunk.sub(output_index, output_index+ BLOCKS_PER_FAST_LOOP*8);

                    // Hand-unrolling for 32 vs 16 or 8 bytes produces yields performance about equivalent
                    // to unsafe pointer code on a Xeon E5-1650v3. 64 byte unrolling was slightly better for
                    // large inputs but significantly worse for 50-byte input, unsurprisingly. I suspect
                    // that it's a not uncommon use case to encode smallish chunks of data (e.g. a 64-byte
                    // SHA-512 digest), so it would be nice if that fit in the unrolled loop at least once.
                    // Plus, single-digit percentage performance differences might well be quite different
                    // on different hardware.

                    let input_u64_0 = read_u64(input_chunk);
                    output_chunk.slice[0] := encode_table_[Nat64.toNat((input_u64_0 >> 58) & LOW_SIX_BITS)];
                    output_chunk.slice[1] := encode_table_[Nat64.toNat((input_u64_0 >> 52) & LOW_SIX_BITS)];
                    output_chunk.slice[2] := encode_table_[Nat64.toNat((input_u64_0 >> 46) & LOW_SIX_BITS)];
                    output_chunk.slice[3] := encode_table_[Nat64.toNat((input_u64_0 >> 40) & LOW_SIX_BITS)];
                    output_chunk.slice[4] := encode_table_[Nat64.toNat((input_u64_0 >> 34) & LOW_SIX_BITS)];
                    output_chunk.slice[5] := encode_table_[Nat64.toNat((input_u64_0 >> 28) & LOW_SIX_BITS)];
                    output_chunk.slice[6] := encode_table_[Nat64.toNat((input_u64_0 >> 22) & LOW_SIX_BITS)];
                    output_chunk.slice[7] := encode_table_[Nat64.toNat((input_u64_0 >> 16) & LOW_SIX_BITS)];

                    let input_u64_1 = read_u64(Array.freeze(Types.sub_array<Nat8>(input_chunk, 6, input_chunk.size()-1)));
                    output_chunk.slice[8] := encode_table_[Nat64.toNat((input_u64_1 >> 58) & LOW_SIX_BITS)];
                    output_chunk.slice[9] := encode_table_[Nat64.toNat((input_u64_1 >> 52) & LOW_SIX_BITS)];
                    output_chunk.slice[10] := encode_table_[Nat64.toNat((input_u64_1 >> 46) & LOW_SIX_BITS)];
                    output_chunk.slice[11] := encode_table_[Nat64.toNat((input_u64_1 >> 40) & LOW_SIX_BITS)];
                    output_chunk.slice[12] := encode_table_[Nat64.toNat((input_u64_1 >> 34) & LOW_SIX_BITS)];
                    output_chunk.slice[13] := encode_table_[Nat64.toNat((input_u64_1 >> 28) & LOW_SIX_BITS)];
                    output_chunk.slice[14] := encode_table_[Nat64.toNat((input_u64_1 >> 22) & LOW_SIX_BITS)];
                    output_chunk.slice[15] := encode_table_[Nat64.toNat((input_u64_1 >> 16) & LOW_SIX_BITS)];

                    let input_u64_2 = read_u64(Array.freeze(Types.sub_array<Nat8>(input_chunk, 12, input_chunk.size()-1)));
                    output_chunk.slice[16] := encode_table_[Nat64.toNat((input_u64_2 >> 58) & LOW_SIX_BITS)];
                    output_chunk.slice[17] := encode_table_[Nat64.toNat((input_u64_2 >> 52) & LOW_SIX_BITS)];
                    output_chunk.slice[18] := encode_table_[Nat64.toNat((input_u64_2 >> 46) & LOW_SIX_BITS)];
                    output_chunk.slice[19] := encode_table_[Nat64.toNat((input_u64_2 >> 40) & LOW_SIX_BITS)];
                    output_chunk.slice[20] := encode_table_[Nat64.toNat((input_u64_2 >> 34) & LOW_SIX_BITS)];
                    output_chunk.slice[21] := encode_table_[Nat64.toNat((input_u64_2 >> 28) & LOW_SIX_BITS)];
                    output_chunk.slice[22] := encode_table_[Nat64.toNat((input_u64_2 >> 22) & LOW_SIX_BITS)];
                    output_chunk.slice[23] := encode_table_[Nat64.toNat((input_u64_2 >> 16) & LOW_SIX_BITS)];

                    let input_u64_3 = read_u64(Array.freeze(Types.sub_array<Nat8>(input_chunk, 18, input_chunk.size()-1)));
                    output_chunk.slice[24] := encode_table_[Nat64.toNat((input_u64_3 >> 58) & LOW_SIX_BITS)];
                    output_chunk.slice[25] := encode_table_[Nat64.toNat((input_u64_3 >> 52) & LOW_SIX_BITS)];
                    output_chunk.slice[26] := encode_table_[Nat64.toNat((input_u64_3 >> 46) & LOW_SIX_BITS)];
                    output_chunk.slice[27] := encode_table_[Nat64.toNat((input_u64_3 >> 40) & LOW_SIX_BITS)];
                    output_chunk.slice[28] := encode_table_[Nat64.toNat((input_u64_3 >> 34) & LOW_SIX_BITS)];
                    output_chunk.slice[29] := encode_table_[Nat64.toNat((input_u64_3 >> 28) & LOW_SIX_BITS)];
                    output_chunk.slice[30] := encode_table_[Nat64.toNat((input_u64_3 >> 22) & LOW_SIX_BITS)];
                    output_chunk.slice[31] := encode_table_[Nat64.toNat((input_u64_3 >> 16) & LOW_SIX_BITS)];

                    output.modify_mut(output_index, output_chunk.get_mut_array());


                    output_index += BLOCKS_PER_FAST_LOOP * 8;
                    input_index += BLOCKS_PER_FAST_LOOP * 6;                    
                };
            };
            // Encode what's left after the fast loop.
            let LOW_SIX_BITS_U8: Nat8 = 0x3F;

            let rem = input.size() % 3;
            let start_of_rem = input.size() - rem;

            // start at the first index not handled by fast loop, which may be 0.
            while (input_index < start_of_rem) {
                let input_chunk = Array.freeze(Types.sub_array<Nat8>(input, input_index, input_index + 3));
                let output_chunk = output;
                output_chunk.sub(output_index, output_index+4);                

                output_chunk.slice[0] := encode_table_[Nat8.toNat(input_chunk[0] >> 2)];
                output_chunk.slice[1] := encode_table_[Nat8.toNat((input_chunk[0] << 4 | input_chunk[1] >> 4) & LOW_SIX_BITS_U8)];
                output_chunk.slice[2] := encode_table_[Nat8.toNat((input_chunk[1] << 2 | input_chunk[2] >> 6) & LOW_SIX_BITS_U8)];
                output_chunk.slice[3] := encode_table_[Nat8.toNat(input_chunk[2] & LOW_SIX_BITS_U8)];

                output.modify_mut(output_index, output_chunk.get_mut_array());

                input_index += 3;
                output_index += 4;
            };

            if (rem == 2) {
                output.slice[output_index] := encode_table_[Nat8.toNat(input[start_of_rem] >> 2)];
                output.slice[output_index + 1] := encode_table_[Nat8.toNat((input[start_of_rem] << 4 | input[start_of_rem + 1] >> 4) & LOW_SIX_BITS_U8)];
                output.slice[output_index + 2] := encode_table_[Nat8.toNat((input[start_of_rem + 1] << 2) & LOW_SIX_BITS_U8)];
                output_index += 3;
            } else if (rem == 1) {
                output.slice[output_index] := encode_table_[Nat8.toNat(input[start_of_rem] >> 2)];
                output.slice[output_index + 1] := encode_table_[Nat8.toNat((input[start_of_rem] << 4) & LOW_SIX_BITS_U8)];
                output_index += 2;
            };
            output_index            
        };

        public func decoded_length_estimate(input_len: Nat) : DecodeEstimate {
            Decode.FastPortableEstimate(input_len)
        };

        public func decode(input: [Nat8], output: Slice.Slice<Nat8>, estimate: DecodeEstimate) : Result.Result<Nat, DecodeError> {
            Decode.decode_helper(input, estimate, output, decode_table_, config_.decode_allow_trailing_bits)
        };

        public func config() : Config {
            config_
        };
    };
};
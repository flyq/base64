import Alphabet "../../alphabet";
import Array "mo:base/Array";
import Nat8 "mo:base/Nat8";
import Nat64 "mo:base/Nat64";

module {
    // public type FastPortable = {
    //     encode_table: [Nat8];
    // };

    // public let DEFAULT_ENGINE = 
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
        var encode_padding_: Bool = true;
        var decode_allow_trailing_bits: Bool = false;

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


    /// Create a `FastPortable` engine from an [Alphabet].
    /// While not very expensive to initialize, ideally these should be cached
    /// if the engine will be used repeatedly.
    public class FastPortable(alphabet: Alphabet.Alphabet, _config: FastPortableConfig) {
        var encode_table_: [Nat8] = encode_table(alphabet);
        var decode_table_: [Nat8] = decode_table(alphabet);
        var config: FastPortableConfig = _config;
    };

    
};
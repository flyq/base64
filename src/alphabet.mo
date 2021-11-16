import Lib "./lib";
import Array "mo:base/Array";
import Blob "mo:base/Blob";
import Text "mo:base/Text";
import Result "mo:base/Result";

module {
    public type ParseAlphabetError = {
        #InvalidLength;
        #DuplicatedByte: Nat8;
        #UnprintableByte: Nat8;
        #ReservedByte: Nat8;
    };
    public let PAD_BYTE: Nat8 = 61; // b'='

    public class Alphabet() {
        let ALPHABET_SIZE: Nat = 64;
        public var symbols: [var Nat8] = Array.init<Nat8>(ALPHABET_SIZE, 0);

        public func from_str_unchecked(alphabet: Text) {
            let source_bytes = Blob.toArray(Text.encodeUtf8(alphabet));
            var index = 0;
            while (index < ALPHABET_SIZE) {
                symbols[index] := source_bytes[index];
                index += 1;
            };
        };

        public func from_str(alphabet: Text) : Result.Result<Alphabet, ParseAlphabetError> {
            let bytes = Blob.toArray(Text.encodeUtf8(alphabet));
            if (bytes.size() != ALPHABET_SIZE) {
                return #err(#InvalidLength);
            };
            var index = 0;
            while (index < ALPHABET_SIZE) {
                let byte = bytes[index];
                if (not (byte >= 32 and byte <= 126)) {
                    return #err(#UnprintableByte(byte));
                };
                if (byte == Lib.PAD_BYTE) {
                    return #err(#ReservedByte(byte));
                };
                // Check for duplicates while staying within what const allows.
                // It's n^2, but only over 64 hot bytes, and only once, so it's likely in the single digit
                // microsecond range.
                var probe_index = index+1;
                while (probe_index < ALPHABET_SIZE) {
                    if (byte == bytes[probe_index]) {
                        return #err(#DuplicatedByte(byte));
                    };
                    probe_index += 1;
                };
                index += 1;
            };
            let a = Alphabet();
            a.from_str_unchecked(alphabet);
            #ok(a)
        };
    };
    
    /// The standard alphabet (uses `+` and `/`).
    /// See [RFC 3548](https://tools.ietf.org/html/rfc3548#section-3).
    public func STANDARD() : Alphabet {
        let a = Alphabet();
        a.from_str_unchecked("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/");
        a
    };

    /// The URL safe alphabet (uses `-` and `_`).
    /// See [RFC 3548](https://tools.ietf.org/html/rfc3548#section-4).
    public func URL_SAFE() : Alphabet {
        let a = Alphabet();
        a.from_str_unchecked("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_");
        a
    };

    /// The `crypt(3)` alphabet (uses `.` and `/` as the first two values).
    /// Not standardized, but folk wisdom on the net asserts that this alphabet is what crypt uses.
    public func CRYPT() : Alphabet {
        let a = Alphabet();
        a.from_str_unchecked("./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
        a
    };

    /// The bcrypt alphabet.
    public func BCRYPT() : Alphabet {
        let a = Alphabet();
        a.from_str_unchecked("./ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789");
        a
    };

    /// The alphabet used in IMAP-modified UTF-7 (uses `+` and `,`).
    /// See [RFC 3501](https://tools.ietf.org/html/rfc3501#section-5.1.3)
    public func IMAP_MUTF7() : Alphabet {
        let a = Alphabet();
        a.from_str_unchecked("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+,");
        a
    };

    /// The alphabet used in BinHex 4.0 files.
    /// See [BinHex 4.0 Definition](http://files.stairways.com/other/binhex-40-specs-info.txt)
    public func BIN_HEX() : Alphabet {
        let a = Alphabet();
        a.from_str_unchecked("!\"#$%&'()*+,-0123456789@ABCDEFGHIJKLMNPQRSTUVXYZ[`abcdehijklmpqr");
        a
    };
};
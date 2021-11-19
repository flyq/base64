import Iter "mo:base/Iter";
import Array "mo:base/Array";
module {

    /// Errors that can occur while decoding.
    public type DecodeError = {
        /// An invalid byte was found in the input. The offset and offending byte are provided.
        #InvalidByte: (Nat, Nat8);
        /// The length of the input is invalid.
        /// A typical cause of this is stray trailing whitespace or other separator bytes.
        /// In the case where excess trailing bytes have produced an invalid length *and* the last byte
        /// is also an invalid base64 symbol (as would be the case for whitespace, etc), `InvalidByte`
        /// will be emitted instead of `InvalidLength` to make the issue easier to debug.
        #InvalidLength;
        /// The last non-padding input symbol's encoded 6 bits have nonzero bits that will be discarded.
        /// This is indicative of corrupted or truncated Base64.
        /// Unlike InvalidByte, which reports symbols that aren't in the alphabet, this error is for
        /// symbols that are in the alphabet but represent nonsensical encodings.        
        #InvalidLastSymbol: (Nat, Nat8);
    };

    public func saturating_sub(a: Nat, b: Nat) : Nat {
        if (a < b) {
            return 0;
        } else {
            a - b;
        };
    };

    public func checked_sub(a: Nat, b: Nat) : ?Nat {
        if (a < b) {
            return null;
        } else {
            ?(a-b)
        };
    };

    public func sub_array<T>(a: [T], start: Nat, end: Nat) : [var T] {
        let res = Array.init<T>(end-start+1, a[0]);
        for (i in Iter.range(start, end)) {
            res[i-start] := a[i];
        };
        res
    };

    public func sub_array_mut<T>(a: [var T], start: Nat, end: Nat) : [var T] {
        let res = Array.init<T>(end-start+1, a[0]);
        for (i in Iter.range(start, end)) {
            res[i-start] := a[i];
        };
        res
    };
};
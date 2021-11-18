import Array "mo:base/Array";

module {
    public class Slice<A>() {
        public var slice: [var A] = [var];
        public var len: Nat = 0;

        public func copy_from_slice(v: [var A]) {
            slice := v;
            len := v.size();
        };

        public func get_array() : [A] {
            if (len == 0) { return []; };
            let v = Array.init<A>(len, slice[0]);
            var i: Nat = 0;
            while (i < len) {
                v[i] := slice[i];
                i += 1;
            };
            Array.freeze<A>(v)
        };

        public func get_mut_array() : [var A] {
            if (len == 0) { return [var]; };
            let v = Array.init<A>(len, slice[0]);
            var i: Nat = 0;
            while (i < len) {
                v[i] := slice[i];
                i += 1;
            };
            v
        };

        public func get_sub(start: Nat, end: Nat) : [A] {
            let v = Array.init<A>(len, slice[0]);
            var i: Nat = 0;
            while (i < end-start+1) {
                v[i] := slice[start+i];
                i += 1;
            };
            Array.freeze<A>(v)
        };

        public func sub(start: Nat, end: Nat) {
            len := end-start+1;
            if (start != 0) {
                let tmp_buf = Array.init<A>(len, slice[0]);
                var i: Nat = 0;
                while (i < len) {
                    tmp_buf[i] := slice[start+i];
                    i += 1;
                };
                slice := tmp_buf;
            };
        };

        public func modify(start: Nat, value: [A]) {
            let len_ = value.size();
            for (i in Iter.range(start, start+len-1)) {
                slice[i] := value[i-start];
            };
            len += len_;
        };

        public func modify_mut(start: Nat, value: [var A]) {
            let len_ = value.size();
            for (i in Iter.range(start, start+len-1)) {
                slice[i] := value[i-start];
            };
            len += len_;
        };
    };
};
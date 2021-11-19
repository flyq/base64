import Array "mo:base/Array";
import Iter "mo:base/Iter";

module {
    public class Slice<A>() {
        public var slice: [var A] = [var];
        public var len: Nat = 0;

        // modify slice & len
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

        // get [start, end], end < len -1
        // not modify
        public func get_sub(start: Nat, end: Nat) : [A] {
            let v = Array.init<A>(end-start+1, slice[0]);
            var i: Nat = 0;
            while (i + start < end+1) {
                v[i] := slice[start+i];
                i += 1;
            };
            Array.freeze<A>(v)
        };

        // modify len & slice, end <= len-1
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

        // modify slice only, start + value.size() - 1 < len - 1
        public func modify(start: Nat, value: [A]) {
            let len_ = value.size();
            for (i in Iter.range(start, start+len_-1)) {
                slice[i] := value[i-start];
            };
        };

        // modify slice only, start + value.size() - 1 < len - 1
        public func modify_mut(start: Nat, value: [var A]) {
            let len_ = value.size();
            for (i in Iter.range(start, start+len_-1)) {
                slice[i] := value[i-start];
            };
        };

        // modify slice & len, new array = sub(0, start-1) + value
        public func splice(start: Nat, value: [A]) {
            let len_ = value.size();
            if (len_ + start <= len) {
                for (i in Iter.range(start, start+len_-1)) {
                    slice[i] := value[i-start];
                };
                len := len_ + start;
            } else {
                len := len_ + start;
                let tmp_buf = Array.init<A>(len, slice[0]);
                for (i in Iter.range(1, len-1)) {
                    if (i < start) {
                        tmp_buf[i] := slice[i];
                    } else {
                        tmp_buf[i] := value[i-start];
                    };
                };
                slice := tmp_buf;
            };
        };
    };
};
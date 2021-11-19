import M "mo:matchers/Matchers";
import Alphabet "../src/alphabet";
import Fast "../src/engine/fast_portable/mod";
import FastD "../src/engine/fast_portable/decode";
import Slice "../src/slice";
import S "mo:matchers/Suite";
import T "mo:matchers/Testable";
import Array "mo:base/Array";
import Nat32 "mo:base/Nat32";
import Nat8 "mo:base/Nat8";
import Nat64 "mo:base/Nat64";
import Char "mo:base/Char";
import Error "mo:base/Error";
import Bool "mo:base/Bool";
import Debug "mo:base/Debug";
import Buffer "mo:base/Buffer";

type ParseAlphabetError = Alphabet.ParseAlphabetError;

type TestParseAlphabetError = {
	data: Text;
    expect: ParseAlphabetError;
};

func equal(a: ParseAlphabetError, b: ParseAlphabetError) : Bool {
	a == b
};

func alphabet_equal(a: Alphabet.Alphabet, b: Alphabet.Alphabet) : Bool {
	Array.equal<Nat8>(Array.freeze(a.symbols), Array.freeze(b.symbols), Nat8.equal)
};

let test_alphabet = S.suite("Test Alphabet", do {
	var tests : Buffer.Buffer<S.Suite> = Buffer.Buffer(16);

  	let test_error_datas = [
		{
			data = "AACDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
			expect = #DuplicatedByte(Nat8.fromNat(Nat32.toNat(Char.toNat32('A'))));
		},
		{
			data = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789//";
			expect = #DuplicatedByte(Nat8.fromNat(Nat32.toNat(Char.toNat32('/'))));
		},
		{
			data = "ABCDEFGHIJKLMNOPQRSTUVWXYZZbcdefghijklmnopqrstuvwxyz0123456789+/";
			expect = #DuplicatedByte(Nat8.fromNat(Nat32.toNat(Char.toNat32('Z'))));
		},
		{
			data = "xxxxxxxxxABCDEFGHIJKLMNOPQRSTUVWXYZZbcdefghijklmnopqrstuvwxyz0123456789+/";
			expect = #InvalidLength;
		},
		{
			data = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+=";
			expect = #ReservedByte(Nat8.fromNat(Nat32.toNat(Char.toNat32('='))));
		},
	];

	for (i in test_error_datas.vals()) {
		let a = Alphabet.Alphabet();
		let res = switch (a.from_str(i.data)) {
			case (#err(e)) { e };
			case (#ok(_)) { assert(false); #InvalidLength }; //just for compile
		};
		tests.add(S.test("alphabet test", equal(i.expect, res), M.equals(T.bool(true))));
	};
	do {
		let data_nom = "BCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
		let data_err = Char.toText(Char.fromNat32(0xc));
		let data_unp = data_err # data_nom;
		let a = Alphabet.Alphabet();
		let res = switch (a.from_str(data_unp)) {
			case (#err(e)) { e };
			case (#ok(_)) { assert(false); #InvalidLength }; //just for compile
		};	
		tests.add(S.test("unprint test", equal(#UnprintableByte(0xc), res), M.equals(T.bool(true))));
	};
	do {
		let data = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
		let a = Alphabet.Alphabet();
		let res = switch (a.from_str(data)) {
			case (#err(_)) { assert(false); a }; //just for compile
			case (#ok(k)) { k }; 
		};
		let expect = Alphabet.STANDARD();
		tests.add(S.test("standard test", alphabet_equal(res, expect), M.equals(T.bool(true))));
	};
	tests.toArray()
});

let test_engine = S.suite("Test Engine", do {
	var tests : Buffer.Buffer<S.Suite> = Buffer.Buffer(8);
	let a = Fast.PAD();
	let b = Fast.NO_PAD();
	b.with_encode_padding(true);
	tests.add(S.test("FastPortableConfig", Bool.equal(a.encode_padding(), b.encode_padding()), M.equals(T.bool(true))));

	tests.add(S.test("read_u64", Nat64.equal(0x1234567890123456, Fast.read_u64([0x12, 0x34, 0x56, 0x78, 0x90, 0x12, 0x34, 0x56])), M.equals(T.bool(true))));
	tests.toArray()
});

let test_slice = S.suite("Test Slice", do {
	var tests : Buffer.Buffer<S.Suite> = Buffer.Buffer(4);
	let a = Slice.Slice<Nat8>();
	let val : [var Nat8] = [var 1, 3, 4];
	a.copy_from_slice(val);
	a.sub(0, 1);
	a.slice[0] := 10;
	let expect: [Nat8] = [10, 3];
	Debug.print("x");
	tests.add(S.test("slice test", Array.equal<Nat8>(expect, a.get_array(), Nat8.equal), M.equals(T.bool(true))));
	tests.toArray()
});

// let test_fast_portable_decode = S.suite("Test fast decode", do {
// 	var tests : Buffer.Buffer<S.Suite> = Buffer.Buffer(4);
// 	let output : [Nat8] = [0, 1, 2, 3, 4, 5, 6, 7];


// 	tests.add(S.test("fast decode test", Array.equal<Nat8>(expect, a.get_array(), Nat8.equal), M.equals(T.bool(true))));

// })

let suite = S.suite("base64", [
	test_alphabet,
	test_engine,
	test_slice
]);

S.run(suite);

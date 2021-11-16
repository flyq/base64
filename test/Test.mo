import M "mo:matchers/Matchers";
import Alphabet "../src/alphabet";
import S "mo:matchers/Suite";
import T "mo:matchers/Testable";
import Array "mo:base/Array";
import Nat32 "mo:base/Nat32";
import Nat8 "mo:base/Nat8";
import Char "mo:base/Char";
import Error "mo:base/Error";
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
		let data_err = Char.toText(Char.fromNat32(Nat32.fromNat(0xc)));
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

let suite = S.suite("base64", [
	test_alphabet
]);

S.run(suite);

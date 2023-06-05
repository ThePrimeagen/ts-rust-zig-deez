
#include <iostream>
#include <utility>
#include <gtest/gtest.h>

#include "lexer.hh"
#include "program.hpp"


auto testEval(std::string str, Value expected)
{
	try {
		Lexer lexer{str};
		auto program = Program::parse(lexer);
		auto val = program->run();

		ASSERT_EQ(val.data, expected.data) << str;
	} catch (const std::exception& ex) {
		FAIL() << ex.what() << " : " << str;
		throw;
	}
}

void runTests(const std::vector<std::pair<std::string, Value>>& tests)
{
	for (const auto& [program, expected] : tests) {
		testEval(program, expected);
	}
}

TEST(TestLexer, TestLetStatements) {

	Lexer lexer {R"XXX(
		let x = 5;
		let y = 10;
		let foobar = 838383;
	)XXX"};

	auto program = Program::parse(lexer);
	ASSERT_EQ(program->statements.size(), 3);

	ASSERT_TRUE(lexer.eof());
}


TEST(TestLexer, TestEvalIntegerExpression) {

	runTests({
		{"5", Value{5}},
		{"10", Value{10}},
		{"-5", Value{-5}},
		{"-10", Value{-10}},
		{"5 + 5 + 5 + 5 - 10", Value{10}},
		{"2 * 2 * 2 * 2 * 2", Value{32}},
		{"-50 + 100 + -50", Value{0}},
		{"5 * 2 + 10", Value{20}},
		{"5 + 2 * 10", Value{25}},
		{"20 + 2 * -10", Value{0}},
		{"50 / 2 * 2 + 10", Value{60}},
		{"2 * (5 + 10)", Value{30}},
		{"3 * 3 * 3 + 10", Value{37}},
		{"3 * (3 * 3) + 10", Value{37}},
		{"(5 + 10 * 2 + 15 / 3) * 2 + -10", Value{50}},
		{"10 / 5", Value{2}},
		{"7 % 3", Value{1}},

		{"127 | 128", Value{255}},
		{"11 & 6", Value{2}},
		{"11 & ~6", Value{9}},
		{"13 ^ 15", Value{2}},
	});
}


TEST(TestLexer, TestEvalBooleanExpression) {

	runTests({
		{"true", Value{true}},
		{"false", Value{false}},
		{"1 < 2", Value{true}},
		{"1 > 2", Value{false}},
		{"1 < 1", Value{false}},
		{"1 > 1", Value{false}},
		{"1 == 1", Value{true}},
		{"1 != 1", Value{false}},
		{"1 == 2", Value{false}},
		{"1 != 2", Value{true}},

		{"true == true", Value{true}},
		{"false == false", Value{true}},
		{"true == false", Value{false}},
		{"true != false", Value{true}},
		{"false != true", Value{true}},
		{"(1 < 2) == true", Value{true}},
		{"(1 < 2) == false", Value{false}},
		{"(1 > 2) == true", Value{false}},
		{"(1 > 2) == false", Value{true}},

		{"true && true", Value{true}},
		{"true && false", Value{false}},
		{"false && false", Value{false}},
		{"true || true", Value{true}},
		{"true || false", Value{true}},
		{"false || false", Value{false}},
	});
}

TEST(TestLexer, TestBangOperator) {

	runTests({
		{"!true", Value{false}},
		{"!false", Value{true}},
		{"!5", Value{false}},
		{"!!true", Value{true}},
		{"!!false", Value{false}},
		{"!!5", Value{true}},
	});
}


TEST(TestLexer, TestIfElseExpressions) {

	runTests({
		{"if (true) { 10 }", Value{10}},
		{"if (false) { 10 }", Value{nil}},
		{"if (1) { 10 }", Value{10}},
		{"if (1 < 2) { 10 }", Value{10}},
		{"if (1 > 2) { 10 }", Value{nil}},
		{"if (1 > 2) { 10 } else { 20 }", Value{20}},
		{"if (1 < 2) { 10 } else { 20 }", Value{10}},
	});
}


TEST(TestLexer, TestReturnStatements) {

	runTests({
		{"return 10;", Value{10}},
		{"return 10; 9;", Value{10}},
		{"return 2 * 5; 9;", Value{10}},
		{"9; return 2 * 5; 9;", Value{10}},
		{R"XXX(
			if (10 > 1) {
				if (10 > 1) {
					return 10;
				}
				return 1;
			}
			)XXX", Value{10}},
	});
}


TEST(TestLexer, TestLetStatements2) {

	runTests({
		{"let a = 5; a;",Value{ 5}},
		{"let a = 5 * 5; a;", Value{25}},
		{"let a = 5; let b = a; b;",Value{ 5}},
		{"let a = 5; let b = a; let c = a + b + 5; c;", Value{15}},
	});
}


TEST(TestLexer, TestFunctionApplication) {

	runTests({
		{"let identity = fn(x) { x; }; identity(5);", Value{5}},
		{"let identity = fn(x) { return x; }; identity(5);", Value{5}},
		{"let double = fn(x) { x * 2; }; double(5);", Value{10}},
		{"let add = fn(x, y) { x + y; }; add(5, 5);", Value{10}},
		{"let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", Value{20}},
		{"fn(x) { x; }(5)", Value{5}},
	});
}


TEST(TestLexer, TestClosures) {

	runTests({
		{R"XXX(
			let newAdder = fn(x) {
				fn(y) { x + y };
			};
			let addTwo = newAdder(2);
			addTwo(2);
			)XXX", Value{4}},
	});
}


TEST(TestLexer, TestStringLiteralExpression) {

	runTests({
		{R"XXX( "hello world" )XXX", Value{"hello world"} },
	});
}

TEST(TestLexer, TestStringConcatenation) {
	runTests({
		{R"XXX( "Hello" + " " + "World!" )XXX", Value{"Hello World!"} },
		//{R"XXX( "Hello" - "World!" )XXX", "unknown operator: STRING - STRING" },
	});
}

TEST(TestLexer, TestBuiltinFunctions) {
	runTests({
		{R"XXX( len("") )XXX", Value{0}},
		{R"XXX( len("four") )XXX", Value{4}},
		{R"XXX( len("hello world") )XXX", Value{11}},
		//{R"XXX( len(1) )XXX", "argument to `len` not supported, got INTEGER"},
		//{R"XXX( len("one", "two") )XXX", "wrong number of arguments. got=2, want=1"},
	});
}

TEST(TestLexer, TestParsingArrayLiterals) {
	runTests({
		{"[1, 2 * 2, 3 + 3]", Value{ std::vector<Value>{ Value{1}, Value{4}, Value{6} } }},
	});
}

TEST(TestLexer, TestArrayIndexExpressions) {
	runTests({
		{ "[1, 2, 3][0]", Value{1} },
		{ "[1, 2, 3][1]", Value{2} },
		{ "[1, 2, 3][2]", Value{3} },
		{ "let i = 0; [1][i];", Value{1} },
		{ "[1, 2, 3][1 + 1];", Value{3} },
		{ "let myArray = [1, 2, 3]; myArray[2];", Value{3} },
		{ "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];", Value{6} },
		{ "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]", Value{2} },
		{ "[1, 2, 3][3]", nil },
		{ "[1, 2, 3][-1]", nil },
	});
}


TEST(TestLexer, TestArrayBuiltinFunction) {
	runTests({
		{ "len([])", Value{0} },
		{ "len([1, 2, 3])", Value{3} },
		{ "first([])", nil },
		{ "first([1, 2, 3])", Value{1} },
		{ "last([])", nil },
		{ "last([1, 2, 3])", Value{3} },
		{ "rest([])", nil },
		{ "rest([1, 2, 3])", Value{ Array{ Value{2}, Value{3} } } },
	});
}

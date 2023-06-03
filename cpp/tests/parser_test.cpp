
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

		ASSERT_EQ(val.index(), expected.index()) << str;
		ASSERT_EQ(val, expected) << str;
	} catch (const std::exception& ex) {
		FAIL() << ex.what() << " : " << str;
		throw;
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

	std::vector<std::pair<std::string, int64_t>> tests {
		{"5", 5},
		{"10", 10},
		{"-5", -5},
		{"-10", -10},
		{"5 + 5 + 5 + 5 - 10", 10},
		{"2 * 2 * 2 * 2 * 2", 32},
		{"-50 + 100 + -50", 0},
		{"5 * 2 + 10", 20},
		{"5 + 2 * 10", 25},
		{"20 + 2 * -10", 0},
		{"50 / 2 * 2 + 10", 60},
		{"2 * (5 + 10)", 30},
		{"3 * 3 * 3 + 10", 37},
		{"3 * (3 * 3) + 10", 37},
		{"(5 + 10 * 2 + 15 / 3) * 2 + -10", 50},
	};
	for (const auto& [program, expected] : tests) {
		testEval(program, expected);
	}
}


TEST(TestLexer, TestEvalBooleanExpression) {

	std::vector<std::pair<std::string, Value>> tests {
		{"true", true},
		{"false", false},
		{"1 < 2", true},
		{"1 > 2", false},
		{"1 < 1", false},
		{"1 > 1", false},
		{"1 == 1", true},
		{"1 != 1", false},
		{"1 == 2", false},
		{"1 != 2", true},

		{"true == true", true},
		{"false == false", true},
		{"true == false", false},
		{"true != false", true},
		{"false != true", true},
		{"(1 < 2) == true", true},
		{"(1 < 2) == false", false},
		{"(1 > 2) == true", false},
		{"(1 > 2) == false", true},
	};
	for (const auto& [program, expected] : tests) {
		testEval(program, expected);
	}
}

TEST(TestLexer, TestBangOperator) {

	std::vector<std::pair<std::string, Value>> tests {
		{"!true", false},
		{"!false", true},
		{"!5", false},
		{"!!true", true},
		{"!!false", false},
		{"!!5", true},
	};
	for (const auto& [program, expected] : tests) {
		testEval(program, expected);
	}
}


TEST(TestLexer, TestIfElseExpressions) {

	std::vector<std::pair<std::string, Value>> tests {
		{"if (true) { 10 }", 10},
		{"if (false) { 10 }", nil},
		{"if (1) { 10 }", 10},
		{"if (1 < 2) { 10 }", 10},
		{"if (1 > 2) { 10 }", nil},
		{"if (1 > 2) { 10 } else { 20 }", 20},
		{"if (1 < 2) { 10 } else { 20 }", 10},
	};
	for (const auto& [program, expected] : tests) {
		testEval(program, expected);
	}
}


TEST(TestLexer, TestReturnStatements) {

	std::vector<std::pair<std::string, Value>> tests {
		{"return 10;", 10},
		{"return 10; 9;", 10},
		{"return 2 * 5; 9;", 10},
		{"9; return 2 * 5; 9;", 10},
		{R"XXX(
			if (10 > 1) {
				if (10 > 1) {
					return 10;
				}
				return 1;
			}
			)XXX", 10,},
	};
	for (const auto& [program, expected] : tests) {
		testEval(program, expected);
	}
}


TEST(TestLexer, TestLetStatements2) {

	std::vector<std::pair<std::string, Value>> tests {
		{"let a = 5; a;", 5},
		{"let a = 5 * 5; a;", 25},
		{"let a = 5; let b = a; b;", 5},
		{"let a = 5; let b = a; let c = a + b + 5; c;", 15},
	};
	for (const auto& [program, expected] : tests) {
		testEval(program, expected);
	}
}


TEST(TestLexer, TestFunctionApplication) {

	std::vector<std::pair<std::string, Value>> tests {
		{"let identity = fn(x) { x; }; identity(5);", 5},
		{"let identity = fn(x) { return x; }; identity(5);", 5},
		{"let double = fn(x) { x * 2; }; double(5);", 10},
		{"let add = fn(x, y) { x + y; }; add(5, 5);", 10},
		{"let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20},
		{"fn(x) { x; }(5)", 5},
	};
	for (const auto& [program, expected] : tests) {
		testEval(program, expected);
	}
}


TEST(TestLexer, TestClosures) {

	std::vector<std::pair<std::string, Value>> tests {
		{R"XXX(
			let newAdder = fn(x) {
				fn(y) { x + y };
			};
			let addTwo = newAdder(2);
			addTwo(2);
			)XXX", 4},
	};
	for (const auto& [program, expected] : tests) {
		testEval(program, expected);
	}
}


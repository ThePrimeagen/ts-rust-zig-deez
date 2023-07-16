#include "tests.hh"
#include "vm/vm.hh"

struct vmTestCase {
	std::string input;
	Object expected;
};

template<std::size_t N >
void runSwitchVmTests(TestHelper &tt, vmTestCase(&tests)[N]) {
	std::optional<std::string> error;
	for (auto iter = std::begin(tests); iter != std::end(tests); ++iter) {
		auto program = parse(std::move(iter->input));
		GlobalData globals;;
		Compiler compiler(&globals);
		error = program->compile(compiler);
		if (error) {
			tt.fail("Fatal - compiler Error: ", *error);
			continue;
		}
		SwitchVM vm(&globals);
		Bytecode bytecode(std::move(compiler));
		error = vm.run(std::move(bytecode));
		if (error) {
			if (error != iter->expected)
				tt.fail("Error executing code: ", *error);
			continue;
		}

		Object result = vm.last_popped_element();
		if (result != iter->expected) {
			tt.fail("Unexpected result, got: ", result, " but expected ", iter->expected);
			continue;
		}
	}
}

void test_arithmetic_exec() {
	vmTestCase tests[] = {
		{"1",1},
		{"2",2},
		{"1 + 2",3},
		{"1 - 2", -1},
		{"1 * 2", 2},
		{"4 / 2", 2},
		{"50 / 2 * 2 + 10 - 5", 55},
		{"5 + 5 + 5 + 5 - 10", 10},
		{"2 * 2 * 2 * 2 * 2", 32},
		{"5 * 2 + 10", 20},
		{"5 + 2 * 10", 25},
		{"5 * (2 + 10)", 60},
		{"-5", -5},
		{"-10", -10},
		{"-50 + 100 + -50", 0},
		{"(5 + 10 * 2 + 15 / 3) * 2 + -10", 50},
		{"0x00FF | 0xFF00", 0xFFFF},
		{"0xFF & 0xFFFF", 0xFF} 
	};
	TestHelper tt("Arithmetic execution");
	runSwitchVmTests(tt, tests);
}

void test_booleans_exec() {
	vmTestCase tests[] = {
		{"true",true},
		{"false",false},
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
		{"!true", false},
		{"!false", true},
		{"!5", false},
		{"!!true", true},
		{"!!false", false},
		{"!!5", true},
		{"!(if (false) { 5; })", true},
	};
	TestHelper tt("Boolean execution");
	runSwitchVmTests(tt, tests);
}

void test_conditionals_exec() {
	vmTestCase tests[] = {
		{"if (true) { 10 }", 10},
		{"if (true) { 10 } else { 20 }", 10},
		{"if (false) { 10 } else { 20 } ", 20},
		{"if (1) { 10 }", 10},
		{"if (1 < 2) { 10 }", 10},
		{"if (1 < 2) { 10 } else { 20 }", 10},
		{"if (1 > 2) { 10 } else { 20 }", 20},
		{"if (1 > 2) { 10 }", Object()},
		{"if (false) { 10 }", Object()},
		{"if ((if (false) { 10 })) { 10 } else { 20 }", 20},
	};
	TestHelper tt("Conditionals execution");
	runSwitchVmTests(tt, tests);
}

void test_global_let_exec() {
	vmTestCase tests[] = {
		{"let one = 1; one", 1},
		{"let one = 1; let two = 2; one + two", 3},
		{"let one = 1; let two = one + one; one + two", 3},
	};
	TestHelper tt("Global Let execution");
	runSwitchVmTests(tt, tests);
}

void test_string_exec() {
	vmTestCase tests[] = {
		{"\"monkey\"", "monkey"},
		{"\"mon\" + \"key\"", "monkey"},
		{"\"mon\" + \"key\" + \"banana\"", "monkeybanana"},
	};
	TestHelper tt("String execution");
	runSwitchVmTests(tt, tests);
}

void test_array_exec() {
	vmTestCase tests[] = {
		{"[]", Object(make_array())},
		{"[1, 2, 3]",Object(make_array({1, 2, 3}))},
		{"[1 + 2, 3 * 4, 5 + 6]",Object(make_array({ 3, 12, 11 }))},
	};
	TestHelper tt("Array execution");
	runSwitchVmTests(tt, tests);
}

void test_hash_exec() {
	vmTestCase tests[] = {
		{"{}", Object(make_hash())},
		{
			"{1: 2, 2: 3}",
			Object(make_hash({ {1, 2}, {2, 3}}))
		},
		{
			"{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
			Object(make_hash({{2,4},{6,16}}))
		},
	};
	TestHelper tt("Hash execution");
	runSwitchVmTests(tt, tests);
}

void test_index_exec() {
	vmTestCase tests[] = {
		{"[1, 2, 3][1]", 2},
		{"[1, 2, 3][0 + 2]", 3},
		{"[[1, 1, 1]][0][0]", 1},
		{"[][0]", Object()},
		{"[1, 2, 3][99]", Object()},
		{"[1][-1]", Object()},
		{"{1: 1, 2: 2}[1]", 1},
		{"{1: 1, 2: 2}[2]", 2},
		{"{1: 1}[0]", Object()},
		{"{}[0]", Object()},
	};
	TestHelper tt("Index execution");
	runSwitchVmTests(tt, tests);
}

void test_function_call_no_args_exec() {
	vmTestCase tests[] = {
		{"let fivePlusTen = fn() { 5 + 10; }; fivePlusTen();", 15},
		{
			"let one = fn() { 1; };\n"
			"let two = fn() { 2; };\n"
			"one() + two()\n", 
			3
		},
		{
			"let a = fn() { 1 };\n"
			"let b = fn() { a() + 1 };\n"
			"let c = fn() { b() + 1 };\n"
			"c();\n", 
			3
		},
		{ "let earlyExit = fn() { return 99; 100; }; earlyExit();", 99 },
		{ "let earlyExit = fn() { return 99; return 100; }; earlyExit();", 99 },
	};
	TestHelper tt("Call function without args execution");
	runSwitchVmTests(tt, tests);
}

void test_function_call_no_retval_exec() {
	vmTestCase tests[] = {
		{ "let noReturn = fn() { }; noReturn();", Object()},
		{ 
			"let noReturn = fn() { };\n"
			"let noReturnTwo = fn() { noReturn(); };\n"
			"noReturn();\n"
			"noReturnTwo();\n", 
			Object()
		},
	};
	TestHelper tt("Call function without return value");
	runSwitchVmTests(tt, tests);
}

void test_function_first_class_exec() {
	vmTestCase tests[] = {
		{
			"let returnsOne = fn() { 1; };\n"
			"let returnsOneReturner = fn() { returnsOne; };\n"
			"returnsOneReturner()();\n",
			1
		},
		{
			"let returnsOneReturner = fn() { fn() { 1; }; };\n"
			"returnsOneReturner()();\n",
			1
		}
	};
	TestHelper tt("Firts class functions");
	runSwitchVmTests(tt, tests);
}

void test_function_calls_with_locals_exec() {
	vmTestCase tests[] = {
		{ "let one = fn() { let one = 1; one }; one();", 1},
		{ "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; }; oneAndTwo();", 3},
		{
			"let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };\n"
			"let threeAndFour = fn() { let three = 3; let four = 4; three + four; };\n"
			"oneAndTwo() + threeAndFour();\n", 10
		},
		{
			"let firstFoobar = fn() { let foobar = 50; foobar; };\n"
			"let secondFoobar = fn() { let foobar = 100; foobar; };\n"
			"firstFoobar() + secondFoobar();\n",
			150
		},
		{
			"let globalSeed = 50;\n"
			"let minusOne = fn() {\n"
			"	let num = 1;\n"
			"	globalSeed - num;\n"
			"}\n"
			"let minusTwo = fn() {\n"
			"	let num = 2;\n"
			"	globalSeed - num;\n"
			"}\n"
			"minusOne() + minusTwo();\n",
			97
		},
	};
	TestHelper tt("Functions with locals");
	runSwitchVmTests(tt, tests);
}

void test_function_calls_with_argument_exec() {
	vmTestCase tests[] = {
		{
			"let identity = fn(a) { a; };\n"
			"identity(4);\n",
			4
		},
		{
			"let sum = fn(a, b) { a + b; };\n"
			"sum(1, 2);\n",
			3
		},
		{
			"let sum = fn(a, b) {\n"
			"	let c = a + b;\n"
			"	c;\n"
			"};\n"
			"sum(1, 2);\n",
			3
		},
		{
			"let sum = fn(a, b) {\n"
			"	let c = a + b;\n"
			"	c;\n"
			"};\n"
			"sum(1, 2) + sum(3, 4);\n",
			10
		},
		{
			"let sum = fn(a, b) {\n"
			"	let c = a + b;\n"
			"	c;\n"
			"};\n"
			"let outer = fn() {\n"
			"	sum(1, 2) + sum(3, 4);\n"
			"}\n"
			"outer();",
			10
		},
		{
			"let globalNum = 10;\n"
			"let sum = fn(a, b) {\n"
			"	let c = a + b;\n"
			"	c + globalNum;\n"
			"};\n"
			"let outer = fn() {\n"
			"	sum(1, 2) + sum(3, 4) + globalNum;\n"
			"};\n"
			"outer() + globalNum;\n",
			50
		}
	};
	TestHelper tt("Functions with arguments");
	runSwitchVmTests(tt, tests);
}

void test_function_calls_with_wrong_argument_exec() {
	vmTestCase tests[] = {
		{
			"fn() { 1; }(1);",
			"Wrong number of arguments, expected 0 but got 1",
		},
		{
			"fn(a) { a; }();",
			"Wrong number of arguments, expected 1 but got 0"
		},
		{
			"fn(a, b) { a + b; }(1);",
			"Wrong number of arguments, expected 2 but got 1",
		},
	};
	TestHelper tt("Functions with wrong arguments");
	runSwitchVmTests(tt, tests);
}

void test_builtin_function_calls_exec() {
	vmTestCase tests[] = {
		{"len(\"\");", 0 },
		{"len(\"four\");", 4 },
		{"len(\"Hello World\");", 11 },
		{"len(1);", Error("Bad Argument") },
		{"len([1, 2, 3]);", 3 },
		{"len([]);", 0 },
		{"puts(\"Hello World\");", Object() },
		{"first([1, 2, 3])", 1},
		{"first([])", Object()},
		{"first(1)", Error("argument to 'first' must be ARRAY, got INTEGER") },
		{"last([1, 2, 3])", 3},
		{"last([])", Object()},
		{"last(1)", Error("argument to 'last' must be ARRAY, got INTEGER")},
		{"rest([1, 2, 3])",make_array({2, 3})},
		{"rest([])", Object()},
		{"push([], 1)", make_array({1})},
		{"push(1, 1)",Error("argument to 'push' must be ARRAY, got INTEGER")},
		{"pop(push([], 1))", make_array({})},

	};
	TestHelper tt("Builtin functions calls");
	runSwitchVmTests(tt, tests);
}

void test_closure_calls_exec() {
	vmTestCase tests[] = {
		{
			"let newClosure = fn(a) {\n"
			"fn() { a; };\n"
			"};\n"
			"let closure = newClosure(99);\n"
			"closure();\n"
			, 99,
		},
		{
			"let newAdder = fn(a, b) {\n"
			"	fn(c) { a + b + c };\n"
			"};\n"
			"let adder = newAdder(1, 2);\n"
			"adder(8);\n"
			, 11 
		},
		{
			"let newAdder = fn(a, b) {\n"
			"	let c = a + b;\n"
			"	fn(d) { c + d };\n"
			"};\n"
			"let adder = newAdder(1, 2);\n"
			"adder(8);\n"
			, 11
		},
		{
			"let newAdderOuter = fn(a, b) {\n"
			"	let c = a + b;\n"
			"	fn(d) {\n"
			"		let e = d + c;\n"
			"		fn(f) { e + f; };\n"
			"	};\n"
			"};\n"
			"let newAdderInner = newAdderOuter(1, 2)\n"
			"let adder = newAdderInner(3);\n"
			"adder(8);\n"
			, 14
		},
		{
			"let a = 1;\n"
			"let newAdderOuter = fn(b) {\n"
			"	fn(c) {\n"
			"		fn(d) { a + b + c + d };\n"
			"	};\n"
			"};\n"
			"let newAdderInner = newAdderOuter(2)\n"
			"let adder = newAdderInner(3);\n"
			"adder(8);\n"
			, 14
		},
		{
			"let newClosure = fn(a, b) {\n"
			"	let one = fn() { a; };\n"
			"	let two = fn() { b; };\n"
			"	fn() { one() + two(); };\n"
			"};\n"
			"let closure = newClosure(9, 90);\n"
			"closure();\n"
			, 99
		}
	};

	TestHelper tt("Closure calls");
	runSwitchVmTests(tt, tests);
}

void test_recursive_closure_calls_exec() {
	vmTestCase tests[] = {
		{
			"let countDown = fn(x) {\n"
			"	if (x == 0) {\n"
			"		return 0; \n"
			"	}\n"
			"	else {\n"
			"		countDown(x - 1);\n"
			"	}\n"
			"};\n"
			"countDown(1);\n"
			, 0
		},
		{
			"let countDown = fn(x) {\n"
			"	if (x == 0) {\n"
			"		return 0;\n"
			"	}\n"
			"	else {\n"
			"		countDown(x - 1);\n"
			"	}\n"
			"};\n"
			"let wrapper = fn() {\n"
			"	countDown(1);\n"
			"};\n"
			"wrapper();\n"
			, 0
		},
		{
			"let wrapper = fn() {\n"
			"	let countDown = fn(x) {\n"
			"		if (x == 0) {\n"
			"			return 0;\n"
			"		}\n"
			"		else {\n"
			"			countDown(x - 1);\n"
			"		}\n"
			"	};\n"
			"	countDown(1);\n"
			"};\n"
			"wrapper();\n"
			, 0
		}
	};

	TestHelper tt("Recursive Closure calls");
	runSwitchVmTests(tt, tests);
}

void test_logic_operators_exec() {
	vmTestCase tests[] = {
		{"true && true ", true},
		{"true && false ", false},
		{"false && true ", false},
		{"false && false", false},
		{"true || true ", true},
		{"true || false ", true },
		{"false || true ", true },
		{"false || false", false},
	};

	TestHelper tt("Logic operators execution");
	runSwitchVmTests(tt, tests);
}

void test_callback_sort_exec() {
	vmTestCase tests[] = {
		{ "sort([3,1,6,2,8,9,0,4],fn(a,b){ a > b });",
			Object(make_array({9, 8, 6, 4, 3, 2, 1, 0}))
		}
	};

	TestHelper tt("Callback sort execution");
	runSwitchVmTests(tt, tests);
}

void test_vm() {
	std::cout << ">>> Virtual Machine tests <<<\n";
	test_arithmetic_exec();
	test_booleans_exec();
	test_conditionals_exec();
	test_global_let_exec();
	test_string_exec();
	test_array_exec();
	test_hash_exec();
	test_index_exec();
	test_function_call_no_args_exec();
	test_function_call_no_retval_exec();
	test_function_first_class_exec();
	test_function_calls_with_locals_exec();
	test_function_calls_with_argument_exec();
	test_function_calls_with_wrong_argument_exec();
	test_builtin_function_calls_exec();
	test_closure_calls_exec();
	test_recursive_closure_calls_exec();
	test_logic_operators_exec();
	test_callback_sort_exec();

	std::cout << std::endl;
}
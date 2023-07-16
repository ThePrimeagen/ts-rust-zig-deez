#include "tests.hh"
#include "vm/code.hh"

void test_make() {
	struct {
		OpCode code;
		std::initializer_list<std::int64_t> operands;
		Instructions expected;
	} tests[] = {
		{ OpConstant, {65534}, {OpConstant, 255, 254}},
		{ OpAdd, {}, {OpAdd}},
		{ OpGetLocal, {255}, {OpGetLocal, 255}},
		{ OpClosure, {65534, 255}, {OpClosure, 255, 254, 255}}
	};

	TestHelper tt("Instruction make_code()");
	for (auto iter = std::begin(tests); iter != std::end(tests); ++iter) {
		Instructions instruction(iter->code, iter->operands);

		if (instruction.size() != iter->expected.size()) {
			tt.fail("Istruction has wrong length, expected ", iter->expected.size(), " but got ", instruction.size());
			continue;
		}

		for (size_t i = 0; i < instruction.size(); ++i) {
			if (instruction[i] != iter->expected[i]) {
				tt.fail("Instruction has wrong byte at position ", i, " expected ", iter->expected[i], "but got ", instruction[i]);
				break;
			}
		}
	}
}

void test_read_operands() {
	struct {
		OpCode op;
		std::initializer_list<std::int64_t> operands;
		ptrdiff_t bytesRead;
	} tests[] = {
		{OpConstant, {65535}, 2},
		{OpClosure, {65535, 255}, 3}
	};
	TestHelper tt("Instruction read_operands()");
	for (auto iter = std::begin(tests); iter != std::end(tests); ++iter) {
		Instructions instr(iter->op, iter->operands);
		const OpCodeDescription &descr = lookupDescription(iter->op);

		Instructions::const_iterator pos = instr.begin();
		++pos;
		Instructions::const_iterator operand_pos = pos;
		std::vector<std::int64_t> result = instr.read_operands(pos, descr);
		ptrdiff_t bytesRead = std::distance(operand_pos, pos);
		if (bytesRead != iter->bytesRead) {
			tt.fail("Wrong number of operands read, expected ", iter->bytesRead, " but bot ", result.size());
			continue;
		}

		auto exp_iter = iter->operands.begin();
		auto got_iter = result.begin();
		for (; got_iter != result.end(); ++exp_iter, ++got_iter) {
			if (*got_iter != *exp_iter)
				tt.fail("Wrong operand read. Expected ", *exp_iter, " but got ", *got_iter);
		}


	}

}

void test_instructions_to_string() {
	Instructions input;
	input.make_code(OpAdd, { });
	input.make_code(OpGetLocal, {1});
	input.make_code(OpConstant, { 2 });
	input.make_code(OpConstant, { 65535 });
	input.make_code(OpClosure, { 65535, 255 });
	std::string expected=("0000 OpAdd\n"
		"0001 OpGetLocal 1\n"
		"0003 OpConstant 2\n"
		"0006 OpConstant 65535\n"
		"0009 OpClosure 65535 255\n");

	TestHelper tt("Instruction to_string();");

	std::string actual = input.to_string();

	if (actual != expected) {
		tt.fail("Instructions incorrectly formatted:\nGot ", actual, "\nExpected: ", expected);
	}

}


struct CompilerTests
{
	std::string input;
	std::vector<Object> constants;
	std::vector<Instructions> instructions;
};

bool testInstructions(TestHelper &tt, const std::vector<Instructions> &expected, const Instructions &actual) {
	Instructions concatted(expected.begin(), expected.end());
	if (actual.size() != concatted.size())
	{
		tt.fail("Wrong instructions length:\nExpected: ", concatted.to_string(), "\ngot: ", actual.to_string());
		return false;
	}

	for (size_t i = 0; i < concatted.size(); ++i)
		if (actual[i] != concatted[i]) {
			tt.fail("Wrong instruction at ", i, ".\nExpected:\n", concatted.to_string(), "\ngot:\n", actual.to_string());
			return false;
		}
	return true;
}

bool testConstants(TestHelper &tt, const std::vector<Object> &expected, const std::vector<Object> &actual) {
	if (expected.size() != actual.size()) {
		tt.fail("Wrong number of constant, expected ", actual.size(), "but got ", expected.size());
		return false;
	}
	for (size_t i = 0; i < expected.size(); ++i) 
		if (actual[i] != expected[i]) {
			tt.fail("Constant ", i, " expected:\n", expected[i], "\nbut got\n", actual[i]);
		}
	return true;
}

template<std::size_t N >
void runCompilerTests(TestHelper &tt, CompilerTests (&tests)[N])
{
	for (auto iter = std::begin(tests); iter != std::end(tests); ++iter) {
		auto program = parse(std::move(iter->input));
		
		GlobalData globals;
		Compiler compiler(&globals);
		std::optional<std::string> error = program->compile(compiler);
		if (error) {
			tt.fail("Fatal - compiler error: ", *error);
			return;
		}

		Bytecode bytecode(std::move(compiler));

		if (!testInstructions(tt, iter->instructions, bytecode.instructions))
			continue;
		if (!testConstants(tt, iter->constants, bytecode.globals->constants))
			continue;
	}
}

void test_integer_arithmentic_gen() {
	CompilerTests tests[] = {
		{
			"1 + 2",
			{ Object(1), Object(2)},
			{ Instructions(OpConstant, {0}), Instructions(OpConstant, {1}), Instructions(OpAdd), Instructions(OpPop)}
		},
		{
			"1; 2;",
			{ Object(1), Object(2)},
			{ Instructions(OpConstant, {0}), Instructions(OpPop), Instructions(OpConstant, {1}), Instructions(OpPop)}
		},
		{
			"1 - 2",
			{ Object(1), Object(2)},
			{ Instructions(OpConstant, {0}), Instructions(OpConstant, {1}), Instructions(OpSub), Instructions(OpPop)}
		},
		{
			"1 * 2",
			{ Object(1), Object(2)},
			{ Instructions(OpConstant, {0}), Instructions(OpConstant, {1}), Instructions(OpMul), Instructions(OpPop)}
		},
		{
			"2 / 1",
			{ Object(2), Object(1)},
			{ Instructions(OpConstant, {0}), Instructions(OpConstant, {1}), Instructions(OpDiv), Instructions(OpPop)}
		},
		{
			"-1",
			{ Object(1)},
			{ Instructions(OpConstant, {0}), Instructions(OpMinus), Instructions(OpPop)}
		},
		{
			"~1",
			{ Object(1)},
			{ Instructions(OpConstant, {0}), Instructions(OpTilde), Instructions(OpPop)}
		},

	};

	TestHelper tt("Arithmetic code generation");

	runCompilerTests(tt, tests);
}

void test_boolean_gen() {
	CompilerTests tests[] = {
		{ "true", {}, {Instructions(OpTrue), Instructions(OpPop)} },
		{ "false", {}, {Instructions(OpFalse), Instructions(OpPop)} },
		{ 
			"1 > 2", 
			{ Object(1), Object(2)},
			{ Instructions(OpConstant, {0}), Instructions(OpConstant, {1}), Instructions(OpGreaterThan), Instructions(OpPop)}
		},
		{ 
			"1 < 2", 
			{ Object(2), Object(1)},
			{ Instructions(OpConstant, {0}), Instructions(OpConstant, {1}), Instructions(OpGreaterThan), Instructions(OpPop)}
		},
		{ 
			"1 == 2", 
			{ Object(1), Object(2)},
			{ Instructions(OpConstant, {0}), Instructions(OpConstant, {1}), Instructions(OpEqual, {}), Instructions(OpPop)}
		},
		{ 
			"1 != 2", 
			{ Object(1), Object(2)},
			{ Instructions(OpConstant, {0}), Instructions(OpConstant, {1}), Instructions(OpNotEqual), Instructions(OpPop)}
		},
		{ 
			"true == false", 
			{ },
			{ Instructions(OpTrue), Instructions(OpFalse), Instructions(OpEqual), Instructions(OpPop)}
		},
		{ 
			"true != false", 
			{ },
			{ Instructions(OpTrue), Instructions(OpFalse), Instructions(OpNotEqual), Instructions(OpPop)}
		},
		{ "!true", {}, {Instructions(OpTrue), Instructions(OpBang), Instructions(OpPop)} },

	};
	TestHelper tt("Boolean code generation");

	runCompilerTests(tt, tests);
}

void test_conditional_gen() {
	CompilerTests tests[] = {
		{
			" if (true) { 10 }; 3333;",
			{ Object(10), Object(3333)},
			{
				Instructions(OpTrue),			// 0000
				Instructions(OpJumpNotTruthy, {10}),// 0001
				Instructions(OpConstant, {0}),	// 0004
				Instructions(OpJump, {11}),		// 0007
				Instructions(OpNull),			// 0010
				Instructions(OpPop),			// 0011
				Instructions(OpConstant, {1}),	// 0012
				Instructions(OpPop)				// 0015
			}
		},
		{
			" if (true) { 10 } else { 20 }; 3333;",
			{ Object(10), Object(20), Object(3333)},
			{
				Instructions(OpTrue),			// 0000
				Instructions(OpJumpNotTruthy, {10}),	// 0001
				Instructions(OpConstant, {0}),	// 0004
				Instructions(OpJump, { 13} ),	// 0007
				Instructions(OpConstant, {1}),	// 0010
				Instructions(OpPop),			// 0013
				Instructions(OpConstant, {2}),	// 0014
				Instructions(OpPop)				// 0017
			}
		}
	};
	TestHelper tt("Conditional code generation");

	runCompilerTests(tt, tests);
}

void test_global_let_gen() {
	CompilerTests tests[] = {
		{
			"let one = 1; let two = 2;",
			{ Object(1), Object(2)},
			{
				Instructions(OpConstant, { 0 }),
				Instructions(OpSetGlobal, { 0 }),
				Instructions(OpConstant, { 1 }),
				Instructions(OpSetGlobal, { 1 })
			}
		},
		{
			"let one = 1; one;",
			{ Object(1)},
			{
				Instructions(OpConstant, { 0 }),
				Instructions(OpSetGlobal, { 0 }),
				Instructions(OpGetGlobal, { 0 }),
				Instructions(OpPop)
			}
		},
		{
			"let one = 1; let two = one; two",
			{ Object(1)},
			{
				Instructions(OpConstant, { 0 }),
				Instructions(OpSetGlobal, { 0 }),
				Instructions(OpGetGlobal, { 0 }),
				Instructions(OpSetGlobal, { 1 }),
				Instructions(OpGetGlobal, { 1 }),
				Instructions(OpPop)
			}
		},
	};
	TestHelper tt("Global Let statement generation");

	runCompilerTests(tt, tests);
	}

void test_string_gen() {
	CompilerTests tests[] = {
		{
			"\"monkey\"",
			{ Object("monkey")},
			{
				Instructions(OpConstant, { 0 }),
				Instructions(OpPop)
			}
		},
		{
			"\"mon\" + \"key\"",
			{ Object("mon"), Object("key")},
			{
				Instructions(OpConstant, { 0 }),
				Instructions(OpConstant, { 1 }),
				Instructions(OpAdd),
				Instructions(OpPop)
			}
		}
	};
	TestHelper tt("String code generation");

	runCompilerTests(tt, tests);
}

void test_array_gen() {
	CompilerTests tests[] = {
		{
			"[]",
			{ },
			{
				Instructions(OpArray, { 0 }),
				Instructions(OpPop)
			}
		},
		{
			"[1, 2, 3]",
			{ Object(1), Object(2), Object(3)},
			{
				Instructions(OpConstant, { 0 }),
				Instructions(OpConstant, { 1 }),
				Instructions(OpConstant, { 2 }),
				Instructions(OpArray, { 3 }),
				Instructions(OpPop)
			}
		},
		{
			"[1 + 2, 3 - 4, 5 * 6]",
			{ Object(1), Object(2), Object(3), Object(4), Object(5), Object(6)},
			{
				Instructions(OpConstant, { 0 }),
				Instructions(OpConstant, { 1 }),
				Instructions(OpAdd),
				Instructions(OpConstant, { 2 }),
				Instructions(OpConstant, { 3 }),
				Instructions(OpSub),
				Instructions(OpConstant, { 4 }),
				Instructions(OpConstant, { 5 }),
				Instructions(OpMul),
				Instructions(OpArray, { 3 }),
				Instructions(OpPop)
			}
		}, 
	};
	TestHelper tt("Array code generation");
	runCompilerTests(tt, tests);
}

void test_hash_gen() {
	CompilerTests tests[] = {
		{
			"{}",
			{ },
			{
				Instructions(OpHash, { 0 }),
				Instructions(OpPop)
			}
		},
		{
			"{1:2, 3:4, 5:6}",
			{ Object(1), Object(2), Object(3), Object(4), Object(5), Object(6)},
			{
				Instructions(OpConstant, { 0 }),
				Instructions(OpConstant, { 1 }),
				Instructions(OpConstant, { 2 }),
				Instructions(OpConstant, { 3 }),
				Instructions(OpConstant, { 4 }),
				Instructions(OpConstant, { 5 }),
				Instructions(OpHash, { 6 }),
				Instructions(OpPop)
			}
		},
		{
			"{1:2+3, 4: 5*6}",
			{ Object(1), Object(2), Object(3), Object(4), Object(5), Object(6)},
			{
				Instructions(OpConstant, { 0 }),
				Instructions(OpConstant, { 1 }),
				Instructions(OpConstant, { 2 }),
				Instructions(OpAdd),
				Instructions(OpConstant, { 3 }),
				Instructions(OpConstant, { 4 }),
				Instructions(OpConstant, { 5 }),
				Instructions(OpMul),
				Instructions(OpHash, { 4 }),
				Instructions(OpPop)
			}
		},
	};
	TestHelper tt("Hash code generation");
	runCompilerTests(tt, tests);
}

void test_index_gen() {
	CompilerTests tests[] = {
		{
			"[1, 2, 3][1 + 1]",
			{ Object(1), Object(2), Object(3), Object(1), Object(1)},
			{
				Instructions(OpConstant, { 0 }),
				Instructions(OpConstant, { 1 }),
				Instructions(OpConstant, { 2 }),
				Instructions(OpArray, { 3 }),
				Instructions(OpConstant, { 3 }),
				Instructions(OpConstant, { 4 }),
				Instructions(OpAdd),
				Instructions(OpIndex),
				Instructions(OpPop)
			}
		},
		{
			"{1:2}[2 - 1]",
			{ Object(1), Object(2), Object(2), Object(1)},
			{
				Instructions(OpConstant, { 0 }),
				Instructions(OpConstant, { 1 }),
				Instructions(OpHash, { 2 }),
				Instructions(OpConstant, { 2 }),
				Instructions(OpConstant, { 3 }),
				Instructions(OpSub),
				Instructions(OpIndex),
				Instructions(OpPop)
			}
		},
	};
	TestHelper tt("Index operator code generation");
	runCompilerTests(tt, tests);
}

void test_function_gen() {
	CompilerTests tests[] = {
		{
			"fn() { return 5 + 10 }",
			{
				Object(5),
				Object(10),
				Object(make_compiled_function({
					Instructions(OpConstant, { 0 }),
					Instructions(OpConstant, { 1 }),
					Instructions(OpAdd),
					Instructions(OpReturnValue)
				}, 0, 0)) 
			},
			{
				Instructions(OpClosure, { 2, 0 }),
				Instructions(OpPop)
			}
		},
		{
			"fn() { 5 + 10 }",
			{ 
				Object(5), 
				Object(10), 
				Object(make_compiled_function({
					Instructions(OpConstant, { 0 }),
					Instructions(OpConstant, { 1 }),
					Instructions(OpAdd),
					Instructions(OpReturnValue)
				}, 0, 0)) },
			{
				Instructions(OpClosure, { 2, 0 }),
				Instructions(OpPop)
			}
		},
		{
			"fn() { 1 ; 2 }",
			{ 
				Object(1), 
				Object(2), 
				Object(make_compiled_function({
					Instructions(OpConstant, { 0 }),
					Instructions(OpPop),
					Instructions(OpConstant, { 1 }),
					Instructions(OpReturnValue)
				}, 0, 0)) },
			{
				Instructions(OpClosure, { 2, 0 }),
				Instructions(OpPop)
			}
		}
	};
	TestHelper tt("Function literal code generation");
	runCompilerTests(tt, tests);
}

void test_empty_function_gen() {
	CompilerTests tests[] = {
		{
			"fn() {}",
			{ Object(make_compiled_function({ Instructions(OpReturn) }, 0, 0)) },
			{
				Instructions(OpClosure, { 0, 0 }),
				Instructions(OpPop)
			}
		}
	};
	TestHelper tt("Empty function literal code generation");
	runCompilerTests(tt, tests);
}

void test_function_call_gen() {
	CompilerTests tests[] = {
		{
			"fn() { 24 }();",
			{  Object(24), Object(make_compiled_function({
				Instructions(OpConstant, { 0 }),
				Instructions(OpReturnValue) 
				}, 0, 0))
			},
			{
				Instructions(OpClosure, { 1, 0 }),
				Instructions(OpCall, { 0 }),
				Instructions(OpPop)
			}
		},
		{
			"let noArgs = fn() { 24 }; noArgs();",
			{  
				Object(24), Object(make_compiled_function({
					Instructions(OpConstant, { 0 }),
					Instructions(OpReturnValue) 
				}, 0, 0))
			},
			{
				Instructions(OpClosure, { 1, 0 }),
				Instructions(OpSetGlobal, { 0 }),
				Instructions(OpGetGlobal, { 0 }),
				Instructions(OpCall, { 0 }),
				Instructions(OpPop)
			}
		},
		{
			"let oneArg = fn(a) { }; oneArg(24);",
			{  
				Object(make_compiled_function({
					Instructions(OpReturn) 
				}, 0, 1)),
				Object(24)
			},
			{
				Instructions(OpClosure, { 0, 0 }),
				Instructions(OpSetGlobal, { 0 }),
				Instructions(OpGetGlobal, { 0 }),
				Instructions(OpConstant, { 1 }),
				Instructions(OpCall, { 1 }),
				Instructions(OpPop)
			}
		},
		{
			"let manyArgs = fn(a,b,c) { }; manyArgs(24,25,26);",
			{  
				Object(make_compiled_function({
					Instructions(OpReturn) 
				}, 0, 3)),
				Object(24),
				Object(25),
				Object(26)
			},
			{
				Instructions(OpClosure, { 0, 0 }),
				Instructions(OpSetGlobal, { 0 }),
				Instructions(OpGetGlobal, { 0 }),
				Instructions(OpConstant, { 1 }),
				Instructions(OpConstant, { 2 }),
				Instructions(OpConstant, { 3 }),
				Instructions(OpCall, { 3 }),
				Instructions(OpPop)
			}
		},
		{
			"let oneArg = fn(a) { a }; oneArg(24);",
			{  
				Object(make_compiled_function({
					Instructions(OpGetLocal, { 0 }),
					Instructions(OpReturnValue)
				},0, 1)),
				Object(24)
			},
			{
				Instructions(OpClosure, { 0, 0 }),
				Instructions(OpSetGlobal, { 0 }),
				Instructions(OpGetGlobal, { 0 }),
				Instructions(OpConstant, { 1 }),
				Instructions(OpCall, { 1 }),
				Instructions(OpPop)
			}
		},
		{
			"let manyArgs = fn(a,b,c) { a; b; c }; manyArgs(24,25,26);",
			{  
				Object(make_compiled_function({
					Instructions(OpGetLocal, { 0 }),
					Instructions(OpPop),
					Instructions(OpGetLocal, { 1 }),
					Instructions(OpPop),
					Instructions(OpGetLocal, { 2 }),
					Instructions(OpReturnValue)
				},0 , 3)),
				Object(24),
				Object(25),
				Object(26)
			},
			{
				Instructions(OpClosure, { 0, 0 }),
				Instructions(OpSetGlobal, { 0 }),
				Instructions(OpGetGlobal, { 0 }),
				Instructions(OpConstant, { 1 }),
				Instructions(OpConstant, { 2 }),
				Instructions(OpConstant, { 3 }),
				Instructions(OpCall, { 3 }),
				Instructions(OpPop)
			}
		}
	};
	TestHelper tt("Function call code generation");
	runCompilerTests(tt, tests);
}

void test_let_statement_scopes_gen() {
	CompilerTests tests[] = {
		{
			"let num = 55; fn() { num }",
			{  Object(55), Object(make_compiled_function({
				Instructions(OpGetGlobal, { 0 }),
				Instructions(OpReturnValue)
				},0 , 0))
			},
			{
				Instructions(OpConstant, { 0 }),
				Instructions(OpSetGlobal, { 0 }),
				Instructions(OpClosure, { 1, 0 }),
				Instructions(OpPop)
			}
		},
		{
			"fn() { let num = 55; num }",
			{  Object(55), Object(make_compiled_function({
				Instructions(OpConstant, { 0 }),
				Instructions(OpSetLocal, { 0 }),
				Instructions(OpGetLocal, { 0 }),
				Instructions(OpReturnValue)
				},0 , 0))
			},
			{
				Instructions(OpClosure, { 1, 0 }),
				Instructions(OpPop)
			}
		},
		{
			"fn() { let a = 55; let b = 77 ; a + b }",
			{  Object(55), Object(77), Object(make_compiled_function({
				Instructions(OpConstant, { 0 }),
				Instructions(OpSetLocal, { 0 }),
				Instructions(OpConstant, { 1 }),
				Instructions(OpSetLocal, { 1 }),
				Instructions(OpGetLocal, { 0 }),
				Instructions(OpGetLocal, { 1 }),
				Instructions(OpAdd),
				Instructions(OpReturnValue)
				}, 2, 0))
			},
			{
				Instructions(OpClosure, { 2, 0 }),
				Instructions(OpPop)
			}
		}
	};
	TestHelper tt("Local scope let statements");
	runCompilerTests(tt, tests);
}

void test_call_builtin_gen() {
	CompilerTests tests[] = {
		{
			"len([]); push([], 1);",
			{ Object(1) },
			{
				Instructions(OpGetBuiltin, { 0 }),
				Instructions(OpArray, { 0 }),
				Instructions(OpCall, { 1 }),
				Instructions(OpPop),
				Instructions(OpGetBuiltin, { 4 }),
				Instructions(OpArray, { 0 }),
				Instructions(OpConstant, { 0 }),
				Instructions(OpCall, { 2 }),
				Instructions(OpPop)
			}
		},
		{
			"fn () { len([]); };",
			{ Object(make_compiled_function({
					Instructions(OpGetBuiltin, { 0 }),
					Instructions(OpArray, { 0 }),
					Instructions(OpCall, { 1 }),
					Instructions(OpReturnValue),
				},0,0))
			},
			{
				Instructions(OpClosure, { 0, 0 }),
				Instructions(OpPop)
			}
		}
	};
	TestHelper tt("Builtin functions calling");
	runCompilerTests(tt, tests);
}

void test_closures_gen() {
	CompilerTests tests[] = {
		{
			"fn(a) { fn(b) { a+b }}",
			{
				Object(make_compiled_function({
					Instructions(OpGetFree, { 0 }),
					Instructions(OpGetLocal, { 0 }),
					Instructions(OpAdd),
					Instructions(OpReturnValue),
					},0,0)),
				Object(make_compiled_function({
					Instructions(OpGetLocal, { 0 }),
					Instructions(OpClosure, { 0, 1 }),
					Instructions(OpReturnValue),
					},0,0)),
			},
			{
				Instructions(OpClosure, { 1, 0 }),
				Instructions(OpPop)
			}
		},
		{
			"fn(a) { fn(b) { fn(c) { a+b+c } } };",
			{
				Object(make_compiled_function({
					Instructions(OpGetFree, { 0 }),
					Instructions(OpGetFree, { 1 }),
					Instructions(OpAdd),
					Instructions(OpGetLocal, { 0 }),
					Instructions(OpAdd),
					Instructions(OpReturnValue),
					},0,0)),
				Object(make_compiled_function({
					Instructions(OpGetFree, { 0 }),
					Instructions(OpGetLocal, { 0 }),
					Instructions(OpClosure, { 0, 2 }),
					Instructions(OpReturnValue),
					},0,0)),
					Object(make_compiled_function({
					Instructions(OpGetLocal, { 0 }),
					Instructions(OpClosure, { 1, 1 }),
					Instructions(OpReturnValue),
					},0,0)),
			},
			{
				Instructions(OpClosure, { 2, 0 }),
				Instructions(OpPop)
			}
		},
		{
			"let global = 55;\n"
			"fn() { let a = 66;\n"
			"	fn() { let b = 77;\n"
			"		fn() { let c = 88;\n"
			"			global + a + b +c;\n"
			"		}\n"
			"	}\n"
			"}\n",
			{ 55, 66, 77, 88, 
				Object(make_compiled_function({
					Instructions(OpConstant, { 3 }),
					Instructions(OpSetLocal, { 0 }),
					Instructions(OpGetGlobal, { 0 }),
					Instructions(OpGetFree, { 0 }),
					Instructions(OpAdd),
					Instructions(OpGetFree, { 1 }),
					Instructions(OpAdd),
					Instructions(OpGetLocal, { 0 }),
					Instructions(OpAdd),
					Instructions(OpReturnValue),
					},0,0)),
				Object(make_compiled_function({
					Instructions(OpConstant, { 2 }),
					Instructions(OpSetLocal, { 0 }),
					Instructions(OpGetFree, { 0 }),
					Instructions(OpGetLocal, { 0 }),
					Instructions(OpClosure, { 4, 2 }),
					Instructions(OpReturnValue),
					},0,0)),
				Object(make_compiled_function({
					Instructions(OpConstant, { 1 }),
					Instructions(OpSetLocal, { 0 }),
					Instructions(OpGetLocal, { 0 }),
					Instructions(OpClosure, { 5, 1 }),
					Instructions(OpReturnValue),
					},0,0)),
			},
			{
				Instructions(OpConstant, { 0 }),
				Instructions(OpSetGlobal, { 0 }),
				Instructions(OpClosure, { 6, 0 }),
				Instructions(OpPop)
			}
		},
	};
	TestHelper tt("Calling Closures");
	runCompilerTests(tt, tests);
}

void test_recursive_closures_gen() {
	CompilerTests tests[] = {
		{
			"let countDown = fn(x) { countDown(x - 1); };\n"
			"countDown(1);\n",
			{
				Object(1),
				Object(make_compiled_function({
					Instructions(OpCurrentClosure),
					Instructions(OpGetLocal, { 0}),
					Instructions(OpConstant, { 0 }),
					Instructions(OpSub),
					Instructions(OpCall, { 1 }),
					Instructions(OpReturnValue)
					}, 1, 1)),
				Object(1)
			},
			{
				Instructions(OpClosure, { 1, 0 }),
				Instructions(OpSetGlobal,{ 0 }),
				Instructions(OpGetGlobal,{ 0 }),
				Instructions(OpConstant, { 2 }),
				Instructions(OpCall, { 1 }),
				Instructions(OpPop),
			}
		},
		{
			"let wrapper = fn() {\n"
			"	let countDown = fn(x) { countDown(x - 1); };\n"
			"	countDown(1);\n"
			"};\n"
			"wrapper();\n",
			{
				Object(1),
				Object(make_compiled_function({
					Instructions(OpCurrentClosure),
					Instructions(OpGetLocal, { 0}),
					Instructions(OpConstant, { 0 }),
					Instructions(OpSub),
					Instructions(OpCall, { 1 }),
					Instructions(OpReturnValue)
					}, 1, 1)),
				Object(1),
				Object(make_compiled_function({
					Instructions(OpClosure, { 1, 0 }),
					Instructions(OpSetLocal, { 0 }),
					Instructions(OpGetLocal, { 0 }),
					Instructions(OpConstant, { 2 }),
					Instructions(OpCall, { 1 }),
					Instructions(OpReturnValue),
				}, 0, 0))
			},
			{
				Instructions(OpClosure, { 3, 0 }),
				Instructions(OpSetGlobal,{ 0 }),
				Instructions(OpGetGlobal,{ 0 }),
				Instructions(OpCall, { 0 }),
				Instructions(OpPop),
			}
			
		}
	};
	TestHelper tt("Recursive Closures");
	runCompilerTests(tt, tests);
}

void test_lobig_operator_gen() {
	CompilerTests tests[] = {
		{
			"true && false",
			{},
			{
				Instructions(OpTrue),			// 0000
				Instructions(OpJumpNotTruthy, { 12 }),   // 0001
				Instructions(OpFalse),			// 0004
				Instructions(OpJumpNotTruthy, { 12 }),   // 0005
				Instructions(OpTrue),			// 0008
				Instructions(OpJump, { 13 }),   // 0009
				Instructions(OpFalse),			// 0012
				Instructions(OpPop),
			}
		},
		{
			"true || false",
			{},
			{
				Instructions(OpTrue),			// 0000
				Instructions(OpJumpTruthy, { 12 }),   // 0001
				Instructions(OpFalse),			// 0004
				Instructions(OpJumpTruthy, { 12 }),   // 0005
				Instructions(OpFalse),			// 0008
				Instructions(OpJump, { 13 }),   // 0009
				Instructions(OpTrue),			// 0012
				Instructions(OpPop),
			}
		}
	};
	TestHelper tt("Logic operators");
	runCompilerTests(tt, tests);
}

void test_code()
{
	std::cout << ">>> ByteCode tests <<<\n";
	test_make();
	test_read_operands();
	test_instructions_to_string();
	test_integer_arithmentic_gen();
	test_boolean_gen();
	test_conditional_gen();
	test_global_let_gen();
	test_string_gen();
	test_array_gen();
	test_hash_gen();
	test_index_gen();
	test_function_gen();
	test_empty_function_gen();
	test_function_call_gen();
	test_let_statement_scopes_gen();
	test_call_builtin_gen();
	test_closures_gen();
	test_recursive_closures_gen();
	test_lobig_operator_gen();

	std::cout << std::endl;
}
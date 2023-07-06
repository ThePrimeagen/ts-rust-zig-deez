#include "tests.hh"

Object test_eval(std::string &&input) {
    lexer l(std::move(input));
    Parser p(l);

    std::shared_ptr<Program> program = p.parse();

    return program->eval(std::make_shared<Environment>());
}

bool test_integer_object(TestHelper &tt, Object object, std::int64_t expected) {

    if (!object.is_a<std::int64_t>()) {
        tt.fail("Expected an integer value!");
        return false;
    }

    if (object.get<std::int64_t>() != expected) {
        tt.fail("Expected a value to be ", expected, " but got ", object);
        return false;
    }
    return true;
}

bool test_boolean_object(TestHelper &tt, Object object, bool expected) {

    if (!object.is_a<bool>()) {
        tt.fail("Expected an boolean value!");
        return false;
    }

    if (object.get<bool>() != expected) {
        tt.fail("Expected a value to be ", expected, " but got ", object);
        return false;
    }
    return true;
}

void test_eval_integer_expression() {
    struct {
        std::string input;
        std::int64_t expected;
    } tests[] = {
        {"5",5},
        {"10",10},
        {"-5",-5},
        {"-10",-10},
        {"-10",-10},
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
        {"(5 + 10 * 2 + 15 / 3) * 2 + -10", 50}
    };

    TestHelper tt("Integer evaluation");
    for (auto iter = std::begin(tests); iter != std::end(tests); ++iter)
    {

        Object result = test_eval(std::move(iter->input));
        if (!test_integer_object(tt, result, iter->expected))
            continue;

    }
}

void test_eval_boolean_expression() {
    struct {
        std::string input;
        bool expected;
    } tests[] = {
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
        {"(1 > 2) == false", true}
    };

    TestHelper tt("Boolean evaluation");
    for (auto iter = std::begin(tests); iter != std::end(tests); ++iter)
    {

        Object result = test_eval(std::move(iter->input));
        if (!test_boolean_object(tt, result, iter->expected))
            continue;

    }
}

void test_eval_bang_expression() {
    struct {
        std::string input;
        bool expected;
    } tests[] = {
        {"!true",false},
        {"!false",true},
        {"!5",false},
        {"!!true",true},
        {"!!false",false},
        {"!!5",true}
    };

    TestHelper tt("Bang expression evaluation");
    for (auto iter = std::begin(tests); iter != std::end(tests); ++iter)
    {

        Object result = test_eval(std::move(iter->input));
        if (!test_boolean_object(tt, result, iter->expected))
            continue;

    }
}

void test_eval_if_else_expression() {
    struct {
        std::string input;
        Object expected;
    } tests[] = {
        {"if (true) { 10 }", 10},
        {"if (false) { 10 }", Object()},
        {"if (1) { 10 }", 10},
        {"if (1 < 2) { 10 }", 10},
        {"if (1 > 2) { 10 }", Object()},
        {"if (1 > 2) { 10 } else { 20 }", 20},
        {"if (1 < 2) { 10 } else { 20 }", 10}
    };
    TestHelper tt("If-Else expression evaluation");
    for (auto iter = std::begin(tests); iter != std::end(tests); ++iter)
    {

        Object result = test_eval(std::move(iter->input));
        if (result != iter->expected)
        {
            tt.fail("Expected value ", iter->expected.to_string(), " but got ", result);
            continue;
        }
    }
}

void test_eval_return_statement() {
    struct {
        std::string input;
        Object expected;
    } tests[] = {
        {"return 10;", 10},
        {"return 10; 9;", 10},
        {"return 2 * 5; 9;", 10},
        {"9; return 2 * 5; 9;", 10},
        {
            "if (10 > 1) {\n"
            "   if (10 > 1) {\n"
            "       return 10;\n"
            "   }\n"
            "   return 1;\n"
            "}\n", 10}
    };
    TestHelper tt("Return statement evaluation");
    for (auto iter = std::begin(tests); iter != std::end(tests); ++iter)
    {

        Object result = test_eval(std::move(iter->input));
        if (result != iter->expected)
        {
            tt.fail("Expected value ", iter->expected.to_string(), " but got ", result);
            continue;
        }
    }
}

void test_eval_error_handling() {
    struct {
        std::string input;
        std::string expected;
    } tests[] = {
        {
            "5 + true;",
            "type mismatch: INTEGER + BOOLEAN",
        },
        {
            "5 + true; 5;",
            "type mismatch: INTEGER + BOOLEAN",
        },
        {
            "-true",
            "unknown operator: -BOOLEAN",
        },
        {
            "true + false;",
            "unknown operator: BOOLEAN + BOOLEAN",
        },
        {
            "5; true + false; 5",
            "unknown operator: BOOLEAN + BOOLEAN",
        },
        {
            "if (10 > 1) { true + false; }",
            "unknown operator: BOOLEAN + BOOLEAN",
        },
        {
            "if (10 > 1) {\n"
            "   if (10 > 1) {\n"
            "       return true + false;\n"
            "   }\n"
            "   return 1;\n"
            "}\n",
            "unknown operator: BOOLEAN + BOOLEAN",
        },
        {
            "\"Hello\" - \"World\"",
            "unknown operator: STRING - STRING"
        }
    };
    TestHelper tt("Error handling");
    for (auto iter = std::begin(tests); iter != std::end(tests); ++iter)
    {

        Object result = test_eval(std::move(iter->input));
        if (!result.isError()) {
            tt.fail("Expected an error value but got ", result);
            continue;
        }
        if (result.get<Error>().message != iter->expected)
        {
            tt.fail("Expected error '", iter->expected, "' but got '", result, '\'');
            continue;
        }
    }
}

void test_eval_let_statement() {
    struct {
        std::string input;
        Object expected;
    } tests[] = {
        {"let a = 5; a;", 5},
        {"let a = 5 * 5; a;", 25},
        {"let a = 5; let b = a; b;", 5},
        {"let a = 5; let b = a; let c = a + b + 5; c;", 15}
    };
    TestHelper tt("Let Statement evaluation");
    for (auto iter = std::begin(tests); iter != std::end(tests); ++iter)
    {

        Object result = test_eval(std::move(iter->input));
        if (result != iter->expected)
        {
            tt.fail("Expected value ", iter->expected.to_string(), " but got ", result);
            continue;
        }
    }
}

void test_eval_function_object() {
    std::string input("fn(x) { x + 2; };");
    TestHelper tt("Function literal evaluation");

    Object result = test_eval(std::move(input));
    if (!result.is_a<func_ptr>())
    {
        tt.fail("Expected a function but got ", result);
        return;
    }

    func_ptr fn = result.get<func_ptr>();

    if (fn->parameters->size() != 1) {
        tt.fail("Unexpected number of parameters: ", fn->parameters->size());
        return;
    }

    if (fn->parameters->at(0) != "x") {
        tt.fail("Parameter is not 'x', got ", fn->parameters->at(0));
        return;
    }

    std::string expectedBody = "{ (x + 2) }"; // TODO: Source implementation does not include braces in block stringification?
    std::string fnbody = fn->body->to_string();
    if (fnbody != expectedBody) {
        tt.fail("Body is  not ", expectedBody, " got ", fnbody);
        return;
    }
}

void test_eval_application() {
    struct {
        std::string input;
        Object expected;
    } tests[] = {
        {"let identity = fn(x) { x; }; identity(5);", 5},
        {"let identity = fn(x) { return x; }; identity(5);", 5},
        {"let double = fn(x) { x * 2; }; double(5);", 10},
        {"let add = fn(x, y) { x + y; }; add(5, 5);", 10},
        {"let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20},
        {"fn(x) { x; }(5)", 5},
    };
    TestHelper tt("Function call evaluation");
    for (auto iter = std::begin(tests); iter != std::end(tests); ++iter)
    {

        Object result = test_eval(std::move(iter->input));
        if (result != iter->expected)
        {
            tt.fail("Expected value ", iter->expected, " but got ", result);
            continue;
        }
    }
}

void test_eval_closure() {
    std::string input(
        "let newAdder = fn(x) {\n"
        "   fn(y) { x + y };\n"
        "};\n"
        "let addTwo = newAdder(2);\n"
        "addTwo(2); ");
    TestHelper tt("Function closure evaluation");

    Object result = test_eval(std::move(input));
    if (result != 4)
    {
        tt.fail("Expected return value 4 but got ", result);
        return;
    }
}

void test_eval_string_expression() {
    struct {
        std::string input;
        std::string expected;
    } tests[] = {
        {"\"foobar\";", "foobar"}
    };

    TestHelper tt("String evaluation");
    for (auto iter = std::begin(tests); iter != std::end(tests); ++iter)
    {

        Object result = test_eval(std::move(iter->input));
        if (!result.is_a<std::string>()) {
            tt.fail("Expected a STRING value, but got a ", result.type_name());
            continue;
        }

        if (result.get<std::string>() != iter->expected) {
            tt.fail("Expected '", iter->expected, "' but got  '", result, '\'');
            continue;
        }

    }
}

void test_eval_string_concatenation() {
    struct {
        std::string input;
        std::string expected;
    } tests[] = {
        {"\"Hello\" + \" \" + \"World\";", "Hello World"}
    };

    TestHelper tt("String concatenation");
    for (auto iter = std::begin(tests); iter != std::end(tests); ++iter)
    {

        Object result = test_eval(std::move(iter->input));
        if (!result.is_a<std::string>()) {
            tt.fail("Expected a STRING value, but got a ", result.type_name());
            continue;
        }
        if (result.get<std::string>() != iter->expected) {
            tt.fail("Expected '", iter->expected, "' but got  '", result, '\'');
            continue;
        }
    }
}

void test_builtin_functions() {
    struct {
        std::string input;
        Object expected;
    } tests[] = {
        {"len(\"\")", 0},
        {"len(\"four\")", 4},
        {"len(\"hello world\")", 11},
        {"len(1)", Error("argument to 'len' not supported, got INTEGER")},
        {"len(\"one\", \"two\")", Error("wrong number of arguments. got=2, want=1")},
        {"len([])", 0},
        {"len([1,2])", 2},
    };

    TestHelper tt("Builtin Functions");
    for (auto iter = std::begin(tests); iter != std::end(tests); ++iter)
    {

        Object result = test_eval(std::move(iter->input));
        if (result != iter->expected) {
            tt.fail("Expected '", iter->expected, "' but got  '", result, '\'');
            continue;
        }

    }
}

void test_eval_array() {
    std::string input("[1, 2 * 2, 3 + 3]");
    TestHelper tt("Array literal evaluation");

    Object result = test_eval(std::move(input));
    if (!result.is_a<array_ptr>())
    {
        tt.fail("Expected an ARRAY but got ", result);
        return;
    }

    array_ptr av = result.get<array_ptr>();

    if (av->size() != 3) {
        tt.fail("Unexpected number of elements in array: ", av->size());
        return;
    }

    auto check = [&](size_t index, Object value) {
        if ((*av)[index] != value) {
            tt.fail("Expected a ", value, " but got  ", (*av)[index]);
            return false;
        }
        return true;
    };

    check(0, 1);
    check(1, 4);
    check(2, 6);
}

void test_array_indexing() {
    struct {
        std::string input;
        Object expected;
    } tests[] = {
        {"[1, 2, 3][0]", 1},
        {"[1, 2, 3][1]", 2},
        {"[1, 2, 3][2]", 3},
        {"let i = 0; [1][i];", 1},
        {"[1, 2, 3][1 + 1];", 3},
        {"let myArray = [1, 2, 3]; myArray[2];", 3},
        {"let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];", 6},
        {"let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]", 2},
        {"[1, 2, 3][3]", Error("Index into array is out of bounds")},
        {"[1, 2, 3][-1]", Error("Index into array is out of bounds")}
    };

    TestHelper tt("Array indexing Functions");
    for (auto iter = std::begin(tests); iter != std::end(tests); ++iter)
    {
        Object result = test_eval(std::move(iter->input));
        if (result != iter->expected) {
            tt.fail("Expected '", iter->expected, "' but got  '", result, '\'');
            continue;
        }

    }
}

void test_eval_hash() {
    std::string input("let two = \"two\";\n"
        "{\n"
        "    \"one\": 10 - 9,\n"
        "        two : 1 + 1,\n"
        "        \"thr\" + \"ee\" : 6 / 2,\n"
        "        4 : 4,\n"
        "        true : 5,\n"
        "        false : 6\n"
        "}\n");
    TestHelper tt("Hash literal evaluation");

    Object result = test_eval(std::move(input));
    if (!result.is_a<hashmap_ptr>())
    {
        tt.fail("Expected a HASH but got ", result);
        return;
    }

    hashmap_ptr hm = result.get<hashmap_ptr>();

    std::vector<std::pair<Object, Object>> expected{
        {"one", 1},
        {"two", 2},
        {"three", 3},
        {4, 4},
        {true, 5},
        {false,  6}
    };

    if (hm->size() != expected.size()) {
        tt.fail("Unexpected number of elements in hash: ", hm->size());
        return;
    }

    for (const auto &expect : expected) {
        auto iter = hm->find(expect.first);
        if (iter == hm->end()) {
            tt.fail("Could not find key '", expect.first, "' in hash");
            continue;
        }
        if (iter->second != expect.second) {
            tt.fail("Expected key '", expect.first, "' to map to '", expect.second, "' but got '", iter->second, '\'');
        }
    }

}

void test_eval_hash_indexing() {
    struct {
        std::string input;
        Object expected;
    } tests[] = {
        { "{\"foo\": 5}[\"foo\"]", 5},
        {"{\"foo\": 5 } [\"bar\"] ",Object()},
        {"let key = \"foo\"; {\"foo\": 5}[key]", 5},
        {"{ }[\"foo\"]",Object()},
        {"{5: 5 }[5]", 5},
        {"{true: 5 }[true]", 5},
        {"{false: 5 }[false]",5}
    };

    TestHelper tt("Hash indexing");
    for (auto iter = std::begin(tests); iter != std::end(tests); ++iter)
    {
        Object result = test_eval(std::move(iter->input));
        if (result != iter->expected) {
            tt.fail("Expected '", iter->expected, "' but got  '", result, '\'');
            continue;
        }
    }
}

void test_eval_callback_sort() {
    std::string input("sort([3,1,6,2,8,9,0,4],fn(a,b){ a>b });");
    std::string expected("[9, 8, 6, 4, 3, 2, 1, 0]");

    TestHelper tt("Callback sorting evaluation");

    Object result = test_eval(std::move(input));
    if (!result.is_a<array_ptr>())
    {
        tt.fail("Expected an ARRAY but got ", result);
        return;
    }

    std::string sorted(result.to_string());

    if (sorted != expected)
        tt.fail("Expected sorted array ", expected, " but got ", sorted);


}

void test_evaluation() {
    std::cout << ">>> Evaluation tests <<<\n";
    test_eval_integer_expression();
    test_eval_boolean_expression();
    test_eval_bang_expression();
    test_eval_if_else_expression();
    test_eval_return_statement();
    test_eval_error_handling();
    test_eval_let_statement();
    test_eval_function_object();
    test_eval_application();
    test_eval_closure();
    test_eval_string_expression();
    test_eval_string_concatenation();
    test_builtin_functions();
    test_eval_array();
    test_array_indexing();
    test_eval_hash();
    test_eval_hash_indexing();
    test_eval_callback_sort();
    std::cout << std::endl;
}

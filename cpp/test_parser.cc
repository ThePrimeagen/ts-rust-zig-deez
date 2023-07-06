#include "tests.hh"

using Literal = std::variant<std::int64_t, std::string, bool>;

bool test_let_statement(LetStatement *stmt, const std::string &ident)
{
    if (stmt->name == nullptr) return false;
    if (stmt->name->name != ident) return false;
    return true;
}

bool test_identifier(TestHelper &tt, Expression *expr, const std::string &value)
{
    Identifier *ident = dynamic_cast<Identifier *>(expr);
    if (ident == nullptr)
    {
        tt.fail("Expected Identifier, got another expression type");
        return false;
    }

    if (ident->name != value) {
        tt.fail("Expected identifier to be named ", value, ", but got ", ident->name);
        return false;
    }
    return true;
}

bool test_integer_literal(TestHelper &tt, Expression *expr, std::int64_t value) {
    IntegerLiteral *intlit = dynamic_cast<IntegerLiteral *>(expr);
    if (intlit == nullptr)
    {
        tt.fail("Expected integer, got another expression type");
        return false;
    }

    if (intlit->value != value) {
        tt.fail("Expected integer with value ", value, ", got ", intlit->value);
        return false;
    }
    return true;
}

bool test_string_literal(TestHelper &tt, Expression *expr, std::string value) {
    StringLiteral *strlit = dynamic_cast<StringLiteral *>(expr);
    if (strlit == nullptr)
    {
        tt.fail("Expected String, got another expression type");
        return false;
    }

    if (strlit->value != value) {
        tt.fail("Expected string with value ", value, ", got ", strlit->value);
        return false;
    }
    return true;
}

bool test_boolean_literal(TestHelper &tt, Expression *expr, bool value) {
    BooleanLiteral *lit = dynamic_cast<BooleanLiteral *>(expr);
    if (lit == nullptr)
    {
        tt.fail("Expected Boolean, got another expression type");
        return false;
    }

    if (lit->value != value) {
        tt.fail("Expected Boolean with value ", value, ", got ", lit->value);
        return false;
    }
    return true;
}

bool test_literal(TestHelper &tt, Expression *expr, const Literal &lit)
{
    if (std::holds_alternative<std::int64_t>(lit)) return test_integer_literal(tt, expr, std::get<std::int64_t>(lit));
    else if (std::holds_alternative<std::string>(lit)) return test_identifier(tt, expr, std::get<std::string>(lit));
    else if (std::holds_alternative<bool>(lit)) return test_boolean_literal(tt, expr, std::get<bool>(lit));
    tt.fail("Unexpected literal type passed to test_literal!");
    return false;
}


bool test_program(TestHelper &tt, std::shared_ptr<Program> &program, size_t stmt_count) {

    if (!program) {
        tt.fail("No program returned");
        return false;
    }
    if (program->statements.size() != stmt_count) {
        tt.fail("Incorrect number of statements returned!");
        return false;
    }
    return true;
}

void test_let_statements()
{
    struct tests {
        std::string input;
        std::string expectedIdentifier;
        Literal value;
    } tests[] = {
        {"let x = 5;", "x", 5},
        {"let y = true;", "y", true},
        {"let foobar = y;", "foobar", "y"}
    };

    TestHelper tt("Let Statements");
    LetStatement *stmt;
    for (auto iter = std::begin(tests); iter != std::end(tests); ++iter) {
        lexer l(std::move(iter->input));
        Parser p(l);

        std::shared_ptr<Program> program = p.parse();

        if (!test_program(tt, program, 1)) return;

        stmt = dynamic_cast<LetStatement *>(program->statements[0].get());
        if (stmt == nullptr)
        {
            tt.fail("Expected let statment, got another statement type");
            return;
        }
        if (!test_let_statement(stmt, iter->expectedIdentifier))
        {
            tt.fail("Expected let statement for identifier ", iter->expectedIdentifier);
            return;
        }
        if (!test_literal(tt, stmt->value.get(), iter->value))
            continue;
    }
}

void test_return_statement()
{
    struct {
        std::string input;
        Literal expected;
    } tests[] = {
        {"return 10;\n",10},
        {"return 993322;\n",993322},
        {"return true;\n",true},
        {"return x;\n","x"}
    };

    TestHelper tt("Return Statement");
    for (auto iter = std::begin(tests); iter != std::end(tests); ++iter) {
        lexer l(std::move(iter->input));
        Parser p(l);

        std::shared_ptr<Program> program = p.parse();

        if (!test_program(tt, program, 1)) return;

        ReturnStatement *stmt = dynamic_cast<ReturnStatement *>(program->statements[0].get());
        if (stmt == nullptr)
        {
            tt.fail("Expected return statment, got another statement type");
            continue;
        }

        if (!test_literal(tt, stmt->value.get(), iter->expected)) continue;
    }
}

void test_identifier_expression() {
    std::string input("foobar;\n");

    TestHelper tt("Identifier");

    lexer l(std::move(input));
    Parser p(l);

    std::shared_ptr<Program> program = p.parse();

    if (!test_program(tt, program, 1)) return;

    ExpressionStatement *es = dynamic_cast<ExpressionStatement *>(program->statements[0].get());
    if (es == nullptr)
    {
        tt.fail("Expected identifier, got another expression type");
        return;
    }

    if (!test_identifier(tt, es->value.get(), "foobar"))
        return;

}

void test_integer_literal() {
    struct {
        std::string input;
        std::int64_t expected;
    } tests[] = {
        {"5;\n",5},
        {"0xF;\n",15},
        {"0xfe;\n",254}
    };

    TestHelper tt("Integer Literal");
    for (auto iter = std::begin(tests); iter != std::end(tests); ++iter) {

        lexer l(std::move(iter->input));
        Parser p(l);

        std::shared_ptr<Program> program = p.parse();

        if (!test_program(tt, program, 1)) return;

        ExpressionStatement *es = dynamic_cast<ExpressionStatement *>(program->statements[0].get());
        if (es == nullptr)
        {
            tt.fail("Expected ExpressionStatement, got another expression type");
            return;
        }

        if (!test_integer_literal(tt, es->value.get(), iter->expected)) {
            return;
        }
    }
}

void test_prefix_parsing() {
    struct test {
        std::string input;
        token_type type;
        std::int64_t value;
    } tests[] = {
        {"!5", token_type::Bang, 5},
        {"-15", token_type::Dash, 15}
    };

    TestHelper tt("Prefix Parsing");
    for (auto iter = std::begin(tests); iter != std::end(tests); ++iter)
    {
        lexer l(std::move(iter->input));
        Parser p(l);

        std::shared_ptr<Program> program = p.parse();

        if (!test_program(tt, program, 1)) continue;

        ExpressionStatement *es = dynamic_cast<ExpressionStatement *>(program->statements[0].get());
        if (es == nullptr)
        {
            tt.fail("Expected identifier, got another expression type");
            return;
        }

        PrefixExpression *prefix = dynamic_cast<PrefixExpression *>(es->value.get());
        if (prefix == nullptr)
        {
            tt.fail("Expected integer, got another expression type");
            return;
        }

        if (prefix->type != iter->type) {
            tt.fail("Unexpected token type in prefix: ", getTokenTypeName(prefix->type));
            return;
        }

        if (!test_integer_literal(tt, prefix->right.get(), iter->value)) {
            return;
        }
    }
}

//bool test_infix_expression(Expression *expr, std::int64_t lhs, token_type tok, std::int64_t rhs) {
bool test_infix_expression(TestHelper &tt, Expression *expr, Literal lhs, token_type tok, Literal  rhs) {
    InfixExpression *infix = dynamic_cast<InfixExpression *>(expr);
    if (infix == nullptr) {
        tt.fail("Expected Infix expression, got another expression type");
        return false;
    }

    if (!test_literal(tt, infix->right.get(), rhs))
        return false;

    if (infix->type != tok) {
        tt.fail("Unexpected token type in prefix: ", getTokenTypeName(infix->type));
        return false;
    }

    if (!test_literal(tt, infix->left.get(), lhs))
        return false;

    return true;
}

void test_infix_expression() {
    struct {
        std::string input;
        std::int64_t left;
        token_type type;
        std::int64_t right;
    } tests[] = {
        {"5 + 5", 5, token_type::Plus, 5},
        {"5 - 5", 5, token_type::Dash, 5},
        {"5 * 5", 5, token_type::Asterisk, 5},
        {"5 / 5", 5, token_type::ForwardSlash, 5},
        {"5 < 5", 5, token_type::LessThan, 5},
        {"5 > 5", 5, token_type::GreaterThan, 5},
        {"5 == 5", 5, token_type::Equal, 5},
        {"6 != 5", 6, token_type::NotEqual, 5},
    };

    TestHelper tt("Infix Expressions");
    for (auto iter = std::begin(tests); iter != std::end(tests); ++iter)
    {
        lexer l(std::move(iter->input));
        Parser p(l);

        std::shared_ptr<Program> program = p.parse();

        if (!test_program(tt, program, 1))
            continue;

        ExpressionStatement *es = dynamic_cast<ExpressionStatement *>(program->statements[0].get());
        if (es == nullptr)
        {
            tt.fail("Expected identifier, got another expression type");
            return;
        }

        if (!test_infix_expression(tt, es->value.get(), iter->left, iter->type, iter->right))
            return;
    }
}

void test_operator_precedence() {
    struct {
        std::string input;
        std::string output;
    } tests[] = {
        {"-a * b","((-a) * b)"},
        {"!-a","(!(-a))"},
        {"a + b + c","((a + b) + c)"},
        {"a + b - c","((a + b) - c)"},
        {"a * b * c","((a * b) * c)"},
        {"a * b / c","((a * b) / c)"},
        {"a + b * c + d / e -f","(((a + (b * c)) + (d / e)) - f)"},
        {"3 + 4; -5 * 5","(3 + 4)((-5) * 5)"},
        {"5 > 4 == 3 < 4","((5 > 4) == (3 < 4))"},
        {"3 + 4 * 5 == 3 * 1 + 4 * 5","((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"},
        {"true", "true" },
        {"false", "false" },
        {"3 > 5 == false", "((3 > 5) == false)" },
        {"3 < 5 == true", "((3 < 5) == true)" },
        {"1 + (2 + 3) + 4","((1 + (2 + 3)) + 4)"},
        {"(5 + 5) * 2","((5 + 5) * 2)"},
        {"2 / (5 + 5)","(2 / (5 + 5))"},
        {"-(5 + 5)","(-(5 + 5))"},
        {"!(true == true)","(!(true == true))"},
        {"a + add(b * c) + d", "((a + add((b * c))) + d)"},
        {"add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"},
        {"add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))"},
        {"a * [1, 2, 3, 4][b * c] * d", "((a * ([1, 2, 3, 4][(b * c)])) * d)"},
        {"add(a * b[2], b[1], 2 * [1, 2][1])", "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))"},
        {"~a", "(~a)"},
        {"a | b", "(a | b)"},
        {"~a | b & c", "((~a) | (b & c))"},
        {"a < b | a < c", "((a < b) | (a < c))"},
        {"a > 0 & a < 100", "((a > 0) & (a < 100))"}
    };

    TestHelper tt("Operator Precedence");
    for (auto iter = std::begin(tests); iter != std::end(tests); ++iter)
    {
        lexer l(std::move(iter->input));
        Parser p(l);

        std::shared_ptr<Program> program = p.parse();

        if (!program) {
            tt.fail("No program returned");
            return;
        }
        std::string output = program->to_string();
        if (output != iter->output) {
            tt.fail("unecpected output, expected ", iter->output, " but got ", output);
            return;
        }
    }
}

void test_if_expression() {
    std::string input("if (x < y) { x }");
    TestHelper tt("If Expression");

    lexer l(std::move(input));
    Parser p(l);
    std::shared_ptr<Program> program = p.parse();

    if (!test_program(tt, program, 1))
        return;

    ExpressionStatement *es = dynamic_cast<ExpressionStatement *>(program->statements[0].get());
    if (es == nullptr) {
        tt.fail("Expected an ExpressionStatement!");
        return;
    }
    IfExpression *ifexpr = dynamic_cast<IfExpression *>(es->value.get());
    if (ifexpr == nullptr) {
        tt.fail("Expected an IfExpression!");
        return;
    }

    if (!test_infix_expression(tt, ifexpr->condition.get(), "x", token_type::LessThan, "y"))
        return;

    BlockStatement *block = dynamic_cast<BlockStatement *>(ifexpr->consequence.get());
    if (block == nullptr) {
        tt.fail("Expected an BlockStatement in consequence!");
        return;
    }
    if (block->statements.size() != 1) {
        tt.fail("Expected exactly 1 statement inconsequence!");
        return;
    }

    es = dynamic_cast<ExpressionStatement *>(block->statements[0].get());
    if (es == nullptr) {
        tt.fail("Expected an ExpressionStatement in consequence!");
        return;
    }

    test_identifier(tt, es->value.get(), "x");
}

void test_if_else_expression() {
    std::string input("if (x < y) { x } else { y }");
    TestHelper tt("If-Else Expression");
    lexer l(std::move(input));
    Parser p(l);

    std::shared_ptr<Program> program = p.parse();

    if (!test_program(tt, program, 1))
        return;

    ExpressionStatement *es = dynamic_cast<ExpressionStatement *>(program->statements[0].get());
    if (es == nullptr) {
        tt.fail("Expected an ExpressionStatement!");
        return;
    }
    IfExpression *ifexpr = dynamic_cast<IfExpression *>(es->value.get());
    if (ifexpr == nullptr) {
        tt.fail("Expected an IfExpression!");
        return;
    }

    if (!test_infix_expression(tt, ifexpr->condition.get(), "x", token_type::LessThan, "y"))
        return;

    BlockStatement *block = dynamic_cast<BlockStatement *>(ifexpr->consequence.get());
    if (block == nullptr) {
        tt.fail("Expected an BlockStatement in consequence!");
        return;
    }
    if (block->statements.size() != 1) {
        tt.fail("Expected exactly 1 staetment inconsequence!");
        return;
    }

    es = dynamic_cast<ExpressionStatement *>(block->statements[0].get());
    if (es == nullptr) {
        tt.fail("Expected an ExpressionStatement in consequence!");
        return;
    }

    if (!test_identifier(tt, es->value.get(), "x")) {
        tt.fail("Expected an ExpressionStatement in consequence!");
        return;
    }

    block = dynamic_cast<BlockStatement *>(ifexpr->alternative.get());
    if (block == nullptr) {
        tt.fail("Expected an BlockStatement in consequence!");
        return;
    }
    if (block->statements.size() != 1) {
        tt.fail("Expected exactly 1 staetment inconsequence!");
        return;
    }

    es = dynamic_cast<ExpressionStatement *>(block->statements[0].get());
    if (es == nullptr) {
        tt.fail("Expected an ExpressionStatement in consequence!");
        return;
    }

    test_identifier(tt, es->value.get(), "y");
}

void test_function_literal() {
    std::string input("fn(x, y) { x + y; }");

    TestHelper tt("Function Literal");
    lexer l(std::move(input));
    Parser p(l);

    std::shared_ptr<Program> program = p.parse();

    if (!test_program(tt, program, 1)) return;

    ExpressionStatement *es = dynamic_cast<ExpressionStatement *>(program->statements[0].get());
    if (es == nullptr) {
        tt.fail("Expected an ExpressionStatement!");
        return;
    }
    FunctionLiteral *fn = dynamic_cast<FunctionLiteral *>(es->value.get());
    if (fn == nullptr) {
        tt.fail("Expected a Function literal!");
        return;
    }
    if (fn->parameters->size() != 2) {
        tt.fail("Expected two parameters to the function!");
        return;
    }

    if ((*fn->parameters)[0] != "x") {
        tt.fail("Expected parameter named x but got ", (*fn->parameters)[0]);
        return;
    }
    if ((*fn->parameters)[1] != "y") {
        tt.fail("Expected parameter named y but got ", (*fn->parameters)[1]);
        return;
    }

    BlockStatement *block = dynamic_cast<BlockStatement *>(fn->body.get());
    if (block == nullptr) {
        tt.fail("Expected an BlockStatement in body!");
        return;
    }
    if (block->statements.size() != 1) {
        tt.fail("Expected exactly 1 staetment inconsequence!");
        return;
    }

    es = dynamic_cast<ExpressionStatement *>(block->statements[0].get());
    if (es == nullptr) {
        tt.fail("Expected an ExpressionStatement in consequence!");
        return;
    }

    test_infix_expression(tt, es->value.get(), "x", token_type::Plus, "y");
}

void test_function_parameter_parsing() {
    struct {
        std::string input;
        std::vector<std::string> params;
    } tests[] = {
        {"fn() {}", {}},
        {"fn(x) {}", {"x"}},
        {"fn(x,y,z) {}", {"x","y","z"}},
    };

    TestHelper tt("Function Parameter parsing");
    for (auto iter = std::begin(tests); iter != std::end(tests); ++iter)
    {
        lexer l(std::move(iter->input));
        Parser p(l);

        std::shared_ptr<Program> program = p.parse();

        if (!test_program(tt, program, 1)) continue;

        ExpressionStatement *es = dynamic_cast<ExpressionStatement *>(program->statements[0].get());
        if (es == nullptr) {
            tt.fail("Expected an ExpressionStatement!");
            return;
        }
        FunctionLiteral *fn = dynamic_cast<FunctionLiteral *>(es->value.get());
        if (fn == nullptr) {
            tt.fail("Expected a Function literal!");
            return;
        }
        if (fn->parameters->size() != iter->params.size()) {
            tt.fail("Expected ", iter->params.size(), " parameters to the function!");
            return;
        }

        for (size_t i = 0; i < iter->params.size(); ++i)
            if ((*fn->parameters)[i] != iter->params[i]) {
                tt.fail("Expected parameter named ", iter->params[i], " but got ", (*fn->parameters)[i]);
                return;
            }
    }
}

void test_call_expression() {
    std::string input("add(1, 2 * 3, 4 + 5);");
    TestHelper tt("Call Expression");
    lexer l(std::move(input));
    Parser p(l);

    std::shared_ptr<Program> program = p.parse();

    if (!test_program(tt, program, 1)) return;

    ExpressionStatement *es = dynamic_cast<ExpressionStatement *>(program->statements[0].get());
    if (es == nullptr) {
        tt.fail("Expected an ExpressionStatement!");
        return;
    }

    CallExpression *ce = dynamic_cast<CallExpression *>(es->value.get());
    if (ce == nullptr) {
        tt.fail("Expected a CallExpression!");
        return;
    }

    if (!test_identifier(tt, ce->function.get(), "add"))
        return;

    if (ce->arguments.size() != 3) {
        tt.fail("Bad number of arguments in call!");
        return;
    }

    if (!test_integer_literal(tt, ce->arguments[0].get(), 1))
        return;
    if (!test_infix_expression(tt, ce->arguments[1].get(), 2, token_type::Asterisk, 3))
        return;
    if (!test_infix_expression(tt, ce->arguments[2].get(), 4, token_type::Plus, 5))
        return;
}

void test_string_expression() {
    std::string input("\"foobar\";\n");

    TestHelper tt("String Literal");

    lexer l(std::move(input));
    Parser p(l);

    std::shared_ptr<Program> program = p.parse();

    if (!test_program(tt, program, 1)) return;

    ExpressionStatement *es = dynamic_cast<ExpressionStatement *>(program->statements[0].get());
    if (es == nullptr)
    {
        tt.fail("Expected identifier, got another expression type");
        return;
    }

    StringLiteral *sl = dynamic_cast<StringLiteral *>(es->value.get());
    if (sl == nullptr)
    {
        tt.fail("Expected String literal, got another expression type");
        return;
    }

    if (sl->value != "foobar") {
        tt.fail("Exprected  string containing 'foobar' but got '", sl->value, "'");
        return;
    }

}

void test_array_expression() {
    std::string input("[1, 2 * 2, 3 + 3];\n");

    TestHelper tt("Array Literal");

    lexer l(std::move(input));
    Parser p(l);

    std::shared_ptr<Program> program = p.parse();

    if (!test_program(tt, program, 1)) return;

    ExpressionStatement *es = dynamic_cast<ExpressionStatement *>(program->statements[0].get());
    if (es == nullptr)
    {
        tt.fail("Expected identifier, got another expression type");
        return;
    }

    ArrayLiteral *al = dynamic_cast<ArrayLiteral *>(es->value.get());
    if (al == nullptr)
    {
        tt.fail("Expected array literal, got another expression type");
        return;
    }
    if (al->elements.size() != 3)
    {
        tt.fail("Expected 3 elements in array, got ", al->elements.size());
        return;
    }

    if (!test_integer_literal(tt, al->elements[0].get(), 1))
        return;
    if (!test_infix_expression(tt, al->elements[1].get(), 2, token_type::Asterisk, 2))
        return;
    if (!test_infix_expression(tt, al->elements[2].get(), 3, token_type::Plus, 3))
        return;
}

void test_index_expression() {
    std::string input("myArray[1 + 1]");

    TestHelper tt("Array Literal");

    lexer l(std::move(input));
    Parser p(l);

    std::shared_ptr<Program> program = p.parse();

    if (!test_program(tt, program, 1)) return;

    ExpressionStatement *es = dynamic_cast<ExpressionStatement *>(program->statements[0].get());
    if (es == nullptr)
    {
        tt.fail("Expected identifier, got another expression type");
        return;
    }

    IndexExpression *ie = dynamic_cast<IndexExpression *>(es->value.get());
    if (ie == nullptr)
    {
        tt.fail("Expected array literal, got another expression type");
        return;
    }

    if (!test_identifier(tt, ie->left.get(), "myArray"))
        return;
    if (!test_infix_expression(tt, ie->index.get(), 1, token_type::Plus, 1))
        return;
}

void test_hash_expression() {
    std::string input("{\"one\": 1, \"two\": 2, \"three\": 3}\n");

    TestHelper tt("Hash Literal");

    lexer l(std::move(input));
    Parser p(l);

    std::shared_ptr<Program> program = p.parse();

    if (!test_program(tt, program, 1)) return;

    ExpressionStatement *es = dynamic_cast<ExpressionStatement *>(program->statements[0].get());
    if (es == nullptr)
    {
        tt.fail("Expected identifier, got another expression type");
        return;
    }

    HashLiteral *hl = dynamic_cast<HashLiteral *>(es->value.get());
    if (hl == nullptr)
    {
        tt.fail("Expected array literal, got another expression type");
        return;
    }
    if (hl->elements.size() != 3)
    {
        tt.fail("Expected 3 elements in hash, got ", hl->elements.size());
        return;
    }

    if (!test_string_literal(tt, hl->elements[0].first.get(), "one"))
        return;
    if (!test_integer_literal(tt, hl->elements[0].second.get(), 1))
        return;
    if (!test_string_literal(tt, hl->elements[1].first.get(), "two"))
        return;
    if (!test_integer_literal(tt, hl->elements[1].second.get(), 2))
        return;
    if (!test_string_literal(tt, hl->elements[2].first.get(), "three"))
        return;
    if (!test_integer_literal(tt, hl->elements[2].second.get(), 3))
        return;
}

void test_empty_expression() {
    std::string input("{}\n");

    TestHelper tt("Empty Hash Literal");

    lexer l(std::move(input));
    Parser p(l);

    std::shared_ptr<Program> program = p.parse();

    if (!test_program(tt, program, 1)) return;

    ExpressionStatement *es = dynamic_cast<ExpressionStatement *>(program->statements[0].get());
    if (es == nullptr)
    {
        tt.fail("Expected identifier, got another expression type");
        return;
    }

    HashLiteral *hl = dynamic_cast<HashLiteral *>(es->value.get());
    if (hl == nullptr)
    {
        tt.fail("Expected array literal, got another expression type");
        return;
    }
    if (hl->elements.size() != 0)
    {
        tt.fail("Expected 0 elements in hash, got ", hl->elements.size());
        return;
    }
}

void test_hash_nested_expression() {
    std::string input("{\"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5}\n");

    TestHelper tt("Hash Literal");

    lexer l(std::move(input));
    Parser p(l);

    std::shared_ptr<Program> program = p.parse();

    if (!test_program(tt, program, 1)) return;

    ExpressionStatement *es = dynamic_cast<ExpressionStatement *>(program->statements[0].get());
    if (es == nullptr)
    {
        tt.fail("Expected identifier, got another expression type");
        return;
    }

    HashLiteral *hl = dynamic_cast<HashLiteral *>(es->value.get());
    if (hl == nullptr)
    {
        tt.fail("Expected array literal, got another expression type");
        return;
    }
    if (hl->elements.size() != 3)
    {
        tt.fail("Expected 3 elements in hash, got ", hl->elements.size());
        return;
    }

    if (!test_string_literal(tt, hl->elements[0].first.get(), "one"))
        return;
    if (!test_infix_expression(tt, hl->elements[0].second.get(), 0, token_type::Plus, 1))
        return;
    if (!test_string_literal(tt, hl->elements[1].first.get(), "two"))
        return;
    if (!test_infix_expression(tt, hl->elements[1].second.get(), 10, token_type::Dash, 8))
        return;
    if (!test_string_literal(tt, hl->elements[2].first.get(), "three"))
        return;
    if (!test_infix_expression(tt, hl->elements[2].second.get(), 15, token_type::ForwardSlash, 5))
        return;
}

void test_parser()
{
    std::cout << ">>> Parser tests <<<\n";
    test_let_statements();
    test_return_statement();
    test_identifier_expression();
    test_integer_literal();
    test_prefix_parsing();
    test_infix_expression();
    test_operator_precedence();
    test_if_expression();
    test_if_else_expression();
    test_function_literal();
    test_function_parameter_parsing();
    test_call_expression();
    test_string_expression();
    test_array_expression();
    test_index_expression();
    test_hash_expression();
    test_empty_expression();
    test_hash_nested_expression();
    std::cout << std::endl;
}

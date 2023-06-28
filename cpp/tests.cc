#include <iostream>
#include <utility>

#include "lexer/lexer.hh"
#include "ast/ast.hh"
#include "parser/parser.hh"

#include <cstdint>
#include <chrono>

using Literal = std::variant<std::int64_t, std::string, bool>;

class TestHelper {
    std::string_view name;
    std::chrono::time_point<std::chrono::steady_clock> start;

    bool failed = false;
public:

    TestHelper(const std::string_view n) : name(n), start(std::chrono::steady_clock::now())
    {
        std::cout << "Starting test '" << name << "'\n";
    };

    ~TestHelper()
    {
        std::chrono::time_point<std::chrono::steady_clock> end = std::chrono::steady_clock::now();
        std::cout << "End of Test '" << name << " - duration: "
            << std::chrono::duration_cast<std::chrono::microseconds>(end - start) << std::endl;

    }
};

void runLexerTest(const std::string &name, lexer &lexer, std::vector<token> expected_tokens)
{
    token token;
    TestHelper tt(name);
    for (auto &expected : expected_tokens) {
        lexer.next_token(token);

        if (token.type != expected.type) {
            std::cerr
                << "Expected token type: "
                << getTokenTypeName(expected.type)
                << " got "
                << getTokenTypeName(token.type)
                << '\n';

            std::exit(1);
        }
        else if (token.literal != expected.literal) {
            std::cerr
                << "Expected token literal: "
                << expected.literal
                << " got "
                << token.literal
                << '\n';

            std::exit(1);
        }
    }

}

void
test_empty_input(void)
{
    std::string input{ "" };

    std::vector<token> expected_tokens{
        {token_type::Eof, "\0"}
    };

    lexer lex{ std::move(input) };

    runLexerTest("Empty Tests", lex, expected_tokens);
    return;
}

void
test_simple_input(void)
{
    std::string input{ "=+(){},;" };

    std::vector<token> expected_tokens{
        {token_type::Assign, "="},
        {token_type::Plus, "+"},
        {token_type::LParen, "("},
        {token_type::RParen, ")"},
        {token_type::LSquirly, "{"},
        {token_type::RSquirly, "}"},
        {token_type::Comma, ","},
        {token_type::Semicolon, ";"},
        {token_type::Eof, "\0"}
    };

    lexer lex{ std::move(input) };

    runLexerTest("Simple Tests", lex, expected_tokens);
    return;
}

void
test_complex_input(void)
{
    std::string input{ "let five = 5;\n"
        "    let ten = 10;\n"
        "    let add = fn(x, y) {\n"
        "        x + y;\n"
        "    };\n"
        "    let result = add(five, ten);\n"
        "!-/*5;\n"
        "5 < 10 > 5;\n"
        "if (5 < 10) {\n"
        "    return true;\n"
        "} else {\n"
        "    return false;\n"
        "}\n"
        "10 == 10;\n"
        "10 != 9;\n"
    };

    std::vector<token> expected_tokens{
        {token_type::Let, "let"},
        {token_type::Identifier, "five"},
        {token_type::Assign, "="},
        {token_type::Integer, "5"},
        {token_type::Semicolon, ";"},
        {token_type::Let, "let"},
        {token_type::Identifier, "ten"},
        {token_type::Assign, "="},
        {token_type::Integer, "10"},
        {token_type::Semicolon, ";"},
        {token_type::Let, "let"},
        {token_type::Identifier, "add"},
        {token_type::Assign, "="},
        {token_type::Function, "fn"},
        {token_type::LParen, "("},
        {token_type::Identifier, "x"},
        {token_type::Comma, ","},
        {token_type::Identifier, "y"},
        {token_type::RParen, ")"},
        {token_type::LSquirly, "{"},
        {token_type::Identifier, "x"},
        {token_type::Plus, "+"},
        {token_type::Identifier, "y"},
        {token_type::Semicolon, ";"},
        {token_type::RSquirly, "}"},
        {token_type::Semicolon, ";"},
        {token_type::Let, "let"},
        {token_type::Identifier, "result"},
        {token_type::Assign, "="},
        {token_type::Identifier, "add"},
        {token_type::LParen, "("},
        {token_type::Identifier, "five"},
        {token_type::Comma, ","},
        {token_type::Identifier, "ten"},
        {token_type::RParen, ")"},
        {token_type::Semicolon, ";"},
        {token_type::Bang, "!"},
        {token_type::Dash, "-"},
        {token_type::ForwardSlash, "/"},
        {token_type::Asterisk, "*"},
        {token_type::Integer, "5"},
        {token_type::Semicolon, ";"},
        {token_type::Integer, "5"},
        {token_type::LessThan, "<"},
        {token_type::Integer, "10"},
        {token_type::GreaterThan, ">"},
        {token_type::Integer, "5"},
        {token_type::Semicolon, ";"},
        {token_type::If, "if"},
        {token_type::LParen, "("},
        {token_type::Integer, "5"},
        {token_type::LessThan, "<"},
        {token_type::Integer, "10"},
        {token_type::RParen, ")"},
        {token_type::LSquirly, "{"},
        {token_type::Return, "return"},
        {token_type::True, "true"},
        {token_type::Semicolon, ";"},
        {token_type::RSquirly, "}"},
        {token_type::Else, "else"},
        {token_type::LSquirly, "{"},
        {token_type::Return, "return"},
        {token_type::False, "false"},
        {token_type::Semicolon, ";"},
        {token_type::RSquirly, "}"},

        {token_type::Integer, "10"},
        {token_type::Equal, "=="},
        {token_type::Integer, "10"},
        {token_type::Semicolon, ";"},

        {token_type::Integer, "10"},
        {token_type::NotEqual, "!="},
        {token_type::Integer, "9"},
        {token_type::Semicolon, ";"},

        {token_type::Eof, "\0"}
    };

    lexer lex{ std::move(input) };
    runLexerTest("Complex Tests", lex, expected_tokens);

    return;
}

void test_lexer()
{
    test_empty_input();
    test_simple_input();
    test_complex_input();
}

void test_to_string() {
    TestHelper tt("to_string method");
    auto program = std::make_unique<Program>();
    auto let = new LetStatement;
    let->name = std::make_unique<Identifier>("myVar");
    let->value = std::make_unique<Identifier>("anotherVar");
    program->statements.push_back(std::unique_ptr<Statement>(let));

    auto result = program->to_string();
    if (result != "let myVar = anotherVar;\n") {
        std::cerr << "Unexpected result from to_string(): " << result << '\n';
        return;
    }

}

bool test_let_statement(LetStatement *stmt, const std::string &ident)
{
    if (stmt->name == nullptr) return false;
    if (stmt->name->name != ident) return false;
    return true;
}

bool test_identifier(Expression *expr, const std::string &value)
{
    Identifier *ident = dynamic_cast<Identifier *>(expr);
    if (ident == nullptr)
    {
        std::cerr << "Expected Identifier, got another expression type\n";
        return false;
    }

    if (ident->name != value) {
        std::cerr << "Expected identifier to be named " << value << ", but got " << ident->name << '\n';
        return false;
    }
    return true;
}

bool test_integer_literal(Expression *expr, std::int64_t value) {
    IntegerLiteral *intlit = dynamic_cast<IntegerLiteral *>(expr);
    if (intlit == nullptr)
    {
        std::cerr << "Expected integer, got another expression type\n";
        return false;
    }

    if (intlit->value != value) {
        std::cerr << "Expected integer with value " << value << ", got " << intlit->value << '\n';
        return false;
    }
    return true;
}

bool test_boolean_literal(Expression *expr, bool value) {
    BooleanLiteral *lit = dynamic_cast<BooleanLiteral *>(expr);
    if (lit == nullptr)
    {
        std::cerr << "Expected Boolean, got another expression type\n";
        return false;
    }

    if (lit->value != value) {
        std::cerr << "Expected Boolean with value " << value << ", got " << lit->value << '\n';
        return false;
    }
    return true;
}

bool test_literal(Expression *expr, const Literal &lit)
{
    if (std::holds_alternative<std::int64_t>(lit)) return test_integer_literal(expr, std::get<std::int64_t>(lit));
    else if (std::holds_alternative<std::string>(lit)) return test_identifier(expr, std::get<std::string>(lit));
    else if (std::holds_alternative<bool>(lit)) return test_boolean_literal(expr, std::get<bool>(lit));
    std::cerr << "Unexpected literal type passed to test_literal!\n";
    return false;
}


bool test_program(std::unique_ptr<Program> &program, size_t stmt_count) {

    if (!program) {
        std::cerr << "No program returned\n";
        return false;
    }

    if (program->statements.size() != stmt_count) {
        std::cerr << "Incorrect number of statements returned!\n";
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
    for (auto iter = std::begin(tests); iter != std::end(tests); ++iter) {
        lexer l(std::move(iter->input));
        Parser p(l);

        std::unique_ptr<Program> program = p.parse();

        if (!test_program(program, 1)) return;

        LetStatement *stmt = dynamic_cast<LetStatement *>(program->statements[0].get());
        if (stmt == nullptr)
        {
            std::cerr << "Expected let statment, got another statement type\n";
            return;
        }
        if (!test_let_statement(stmt, iter->expectedIdentifier))
        {
            std::cerr << "Expected let statement for identifier " << iter->expectedIdentifier << '\n';
            return;
        }
        if (!test_literal(stmt->value.get(), iter->value))
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

        std::unique_ptr<Program> program = p.parse();

        if (!test_program(program, 1)) return;


        ReturnStatement *stmt = dynamic_cast<ReturnStatement *>(program->statements[0].get());
        if (stmt == nullptr)
        {
            std::cerr << "Expected return statment, got another statement type\n";
            return;
        }

        if (!test_literal(stmt->value.get(), iter->expected)) continue;
    }
}

void test_identifier_expression() {
    std::string input("foobar;\n");

    TestHelper tt("Identifier");

    lexer l(std::move(input));
    Parser p(l);

    std::unique_ptr<Program> program = p.parse();

    if (!test_program(program, 1)) return;

    ExpressionStatement *es = dynamic_cast<ExpressionStatement *>(program->statements[0].get());
    if (es == nullptr)
    {
        std::cerr << "Expected identifier, got another expression type\n";
        return;
    }

    if (!test_identifier(es->value.get(),"foobar"))
        return;

}

void test_integer_literal() {
    std::string input("5;\n");

    TestHelper tt("Integer Literal");
    lexer l(std::move(input));
    Parser p(l);

    std::unique_ptr<Program> program = p.parse();

    if (!test_program(program, 1)) return;

    ExpressionStatement *es = dynamic_cast<ExpressionStatement *>(program->statements[0].get());
    if (es == nullptr)
    {
        std::cerr << "Expected ExpressionStatement, got another expression type\n";
        return;
    }

    if (!test_integer_literal(es->value.get(), 5)) {
        return;
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

        std::unique_ptr<Program> program = p.parse();

        if (!test_program(program, 1)) continue;

        ExpressionStatement *es = dynamic_cast<ExpressionStatement *>(program->statements[0].get());
        if (es == nullptr)
        {
            std::cerr << "Expected identifier, got another expression type\n";
            return;
        }

        PrefixExpression *prefix= dynamic_cast<PrefixExpression*>(es->value.get());
        if (prefix == nullptr)
        {
            std::cerr << "Expected integer, got another expression type\n";
            return;
        }

        if (prefix->type != iter->type) {
            std::cerr << "Unexpected token type in prefix: " << getTokenTypeName(prefix->type) << '\n';
            return;
        }
        
        if (!test_integer_literal(prefix->right.get(),iter->value)) {
            return;
        }
    }
}

//bool test_infix_expression(Expression *expr, std::int64_t lhs, token_type tok, std::int64_t rhs) {
bool test_infix_expression(Expression *expr, Literal lhs, token_type tok, Literal  rhs) {
    InfixExpression *infix = dynamic_cast<InfixExpression *>(expr);
    if (infix == nullptr)
    {
        std::cerr << "Expected Infix expression, got another expression type\n";
        return false;
    }

    if (!test_literal(infix->right.get(), rhs)) {
        return false;
    }

    if (infix->type != tok) {
        std::cerr << "Unexpected token type in prefix: " << getTokenTypeName(infix->type) << '\n';
        return false;
    }

    if (!test_literal(infix->left.get(), lhs)) {
        return false;
    }

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

        std::unique_ptr<Program> program = p.parse();

        if (!test_program(program, 1)) continue;

        ExpressionStatement *es = dynamic_cast<ExpressionStatement *>(program->statements[0].get());
        if (es == nullptr)
        {
            std::cerr << "Expected identifier, got another expression type\n";
            return;
        }

        if (!test_infix_expression(es->value.get(), iter->left, iter->type, iter->right))
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
        {"add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))"}
    };

    TestHelper tt("Operator Precedence");
    for (auto iter = std::begin(tests); iter != std::end(tests); ++iter)
    {
        lexer l(std::move(iter->input));
        Parser p(l);

        std::unique_ptr<Program> program = p.parse();

        if (!program) {
            std::cerr << "No program returned\n";
            return;
        }
        std::string output = program->to_string();
        if (output != iter->output) {
            std::cerr << "unecpected output, expected " << iter->output << " but got " << output << '\n';
            return;
        }
    }
}

void test_if_expression() {
    std::string input("if (x < y) { x }");
    TestHelper tt("If Expression");

    lexer l(std::move(input));
    Parser p(l);
    std::unique_ptr<Program> program = p.parse();

    if (!test_program(program, 1)) return;

    ExpressionStatement *es = dynamic_cast<ExpressionStatement *>(program->statements[0].get());
    if (es == nullptr) {
        std::cerr << "Expected an ExpressionStatement!";
        return;
    }
    IfExpression *ifexpr = dynamic_cast<IfExpression *>(es->value.get());
    if (ifexpr == nullptr) {
        std::cerr << "Expected an IfExpression!";
        return;
    }

    if (!test_infix_expression(ifexpr->condition.get(),"x",token_type::LessThan,"y"))
        return;

    BlockStatement *block = dynamic_cast<BlockStatement *>(ifexpr->consequence.get());
    if (block == nullptr) {
        std::cerr << "Expected an BlockStatement in consequence!";
        return;
    }
    if (block->statements.size() != 1) {
        std::cerr << "Expected exactly 1 staetment inconsequence!";
        return;
    }

    es = dynamic_cast<ExpressionStatement *>(block->statements[0].get());
    if (es == nullptr) {
        std::cerr << "Expected an ExpressionStatement in consequence!";
        return;
    }

    if (!test_identifier(es->value.get(), "x")) {
        std::cerr << "Expected an ExpressionStatement in consequence!";
        return;
    }
}

void test_if_else_expression() {
    std::string input("if (x < y) { x } else { y }");
    TestHelper tt("If-Else Expression");
    lexer l(std::move(input));
    Parser p(l);

    std::unique_ptr<Program> program = p.parse();

    if (!test_program(program, 1)) return;

    ExpressionStatement *es = dynamic_cast<ExpressionStatement *>(program->statements[0].get());
    if (es == nullptr) {
        std::cerr << "Expected an ExpressionStatement!";
        return;
    }
    IfExpression *ifexpr = dynamic_cast<IfExpression *>(es->value.get());
    if (ifexpr == nullptr) {
        std::cerr << "Expected an IfExpression!";
        return;
    }

    if (!test_infix_expression(ifexpr->condition.get(), "x", token_type::LessThan, "y"))
        return;

    BlockStatement *block = dynamic_cast<BlockStatement *>(ifexpr->consequence.get());
    if (block == nullptr) {
        std::cerr << "Expected an BlockStatement in consequence!";
        return;
    }
    if (block->statements.size() != 1) {
        std::cerr << "Expected exactly 1 staetment inconsequence!";
        return;
    }

    es = dynamic_cast<ExpressionStatement *>(block->statements[0].get());
    if (es == nullptr) {
        std::cerr << "Expected an ExpressionStatement in consequence!";
        return;
    }

    if (!test_identifier(es->value.get(), "x")) {
        std::cerr << "Expected an ExpressionStatement in consequence!";
        return;
    }

    block = dynamic_cast<BlockStatement *>(ifexpr->alternative.get());
    if (block == nullptr) {
        std::cerr << "Expected an BlockStatement in consequence!";
        return;
    }
    if (block->statements.size() != 1) {
        std::cerr << "Expected exactly 1 staetment inconsequence!";
        return;
    }

    es = dynamic_cast<ExpressionStatement *>(block->statements[0].get());
    if (es == nullptr) {
        std::cerr << "Expected an ExpressionStatement in consequence!";
        return;
    }

    if (!test_identifier(es->value.get(), "y")) {
        std::cerr << "Expected an ExpressionStatement in consequence!";
        return;
    }
}

void test_function_literal() {
    std::string input("fn(x, y) { x + y; }");

    TestHelper tt("Function Literal");
    lexer l(std::move(input));
    Parser p(l);

    std::unique_ptr<Program> program = p.parse();

    if (!test_program(program, 1)) return;

    ExpressionStatement *es = dynamic_cast<ExpressionStatement *>(program->statements[0].get());
    if (es == nullptr) {
        std::cerr << "Expected an ExpressionStatement!";
        return;
    }
    FunctionLiteral *fn = dynamic_cast<FunctionLiteral *>(es->value.get());
    if (fn == nullptr) {
        std::cerr << "Expected a Function literal!";
        return;
    }
    if (fn->parameters.size() != 2) {
        std::cerr << "Expected two parameters to the function!";
        return;
    }

    if (!test_identifier(fn->parameters[0].get(), "x")) return;
    if (!test_identifier(fn->parameters[1].get(), "y")) return;

    BlockStatement *block = dynamic_cast<BlockStatement *>(fn->body.get());
    if (block == nullptr) {
        std::cerr << "Expected an BlockStatement in body!";
        return;
    }
    if (block->statements.size() != 1) {
        std::cerr << "Expected exactly 1 staetment inconsequence!";
        return;
    }

    es = dynamic_cast<ExpressionStatement *>(block->statements[0].get());
    if (es == nullptr) {
        std::cerr << "Expected an ExpressionStatement in consequence!";
        return;
    }

    if (!test_infix_expression(es->value.get(), "x", token_type::Plus, "y"))
        return;
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

        std::unique_ptr<Program> program = p.parse();

        if (!test_program(program, 1)) continue;

        ExpressionStatement *es = dynamic_cast<ExpressionStatement *>(program->statements[0].get());
        if (es == nullptr) {
            std::cerr << "Expected an ExpressionStatement!";
            return;
        }
        FunctionLiteral *fn = dynamic_cast<FunctionLiteral *>(es->value.get());
        if (fn == nullptr) {
            std::cerr << "Expected a Function literal!";
            return;
        }
        if (fn->parameters.size() != iter->params.size()) {
            std::cerr << "Expected " << iter->params.size() << " parameters to the function!";
            return;
        }

        for (size_t i = 0; i < iter->params.size(); ++i)
            if (!test_identifier(fn->parameters[i].get(), iter->params[i])) return;

    }
}

void test_call_expression() {
    std::string input("add(1, 2 * 3, 4 + 5);");
    TestHelper tt("Call Expression");
    lexer l(std::move(input));
    Parser p(l);

    std::unique_ptr<Program> program = p.parse();

    if (!test_program(program, 1)) return;

    ExpressionStatement *es = dynamic_cast<ExpressionStatement *>(program->statements[0].get());
    if (es == nullptr) {
        std::cerr << "Expected an ExpressionStatement!";
        return;
    }

    CallExpression *ce = dynamic_cast<CallExpression *>(es->value.get());
    if (ce == nullptr) {
        std::cerr << "Expected a CallExpression!";
        return;
    }

    if (!test_identifier(ce->function.get(), "add"))
        return;

    if (ce->arguments.size() != 3) {
        std::cout << "Bad number of arguments in call!\n";
        return;
    }

    if (!test_integer_literal(ce->arguments[0].get(), 1))
        return;
    if (!test_infix_expression(ce->arguments[1].get(), 2, token_type::Asterisk, 3)) 
        return;
    if (!test_infix_expression(ce->arguments[2].get(), 4, token_type::Plus, 5)) 
        return;
}

void test_parser()
{
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
}

int
main(void)
{
    test_lexer();
    test_to_string();
    test_parser();
    return 0;
}

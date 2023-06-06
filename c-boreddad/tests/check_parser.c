#include <check.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include "../lexer.h"
#include "../token.h"
#include "../ast.h"
#include "../parser.h"

START_TEST(test_it_works)
{

    char* input = "\
let x = 5;\n\
let y = 10;\n\
let foobar = 838383\
";
    Lexer* l;
    Parser* p;
    Program* program;
    size_t i, len;
    char* tests[3] = {"x", "y", "foobar"};

    l = lexer_new(input);
    ck_assert_ptr_nonnull(l);
    p = parser_new(l);
    ck_assert_ptr_nonnull(p);

    program = parse_program(p);
    ck_assert_ptr_nonnull(program);
    len = program_statements_len(program);
    ck_assert_uint_eq(len, 3);

    for (i = 0; i < 3; i++)
    {
        LetStatement* stmt;
        ck_assert_uint_eq(program->statements->values[i]->type, LETSTATEMENT);
        stmt = program->statements->values[i]->value;
        ck_assert_str_eq(stmt->tok->literal, "let");
        ck_assert_str_eq(stmt->name->value, tests[i]);
    }


    free(l);
    free(p);
    free(program);

    ck_assert_uint_eq(1, 1);
}
END_TEST

START_TEST(test_errors)
{
    char* input = "\
let x = 5;\n\
let = 10\n\
let foobar = 838383\
";
    Lexer* l;
    Parser* p;
    Program* program;
    ParserErorrs* errs;
    size_t i, len;

    l = lexer_new(input);
    ck_assert_ptr_nonnull(l);
    p = parser_new(l);
    ck_assert_ptr_nonnull(p);

    program = parse_program(p);
    ck_assert_ptr_nonnull(program);

    errs = p->errors;
    len = parser_errors_length(p);

    for (i = 0; i < len; i++)
    {
        printf("%s\n", errs->values[i]);
    }

    ck_assert_uint_eq(1, 1);
}
END_TEST

Suite* ht_suite()
{
    Suite* s;
    TCase* tc_core;
    s = suite_create("lexer");
    tc_core = tcase_create("Core");
    tcase_add_test(tc_core, test_it_works);
    tcase_add_test(tc_core, test_errors);
    suite_add_tcase(s, tc_core);
    return s;
}

int main()
{
    int number_failed;
    Suite* s;
    SRunner *sr;
    s = ht_suite();
    sr = srunner_create(s);
    srunner_run_all(sr, CK_NORMAL);
    number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);
    return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}


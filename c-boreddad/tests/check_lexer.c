/* warning: due to how c89 is, this file is extremely shitty. proceed with caution */

#include <check.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include "../lexer.h"
#include "../token.h"

typedef struct{
    TokenT exp_type;
    char* literal;
}Test;

START_TEST(test_it_works)
{
    Test tests[9];
    Test t1, t2, t3, t4, t5, t6, t7, t8, t9;
    char* input = "=+(){},;";
    Lexer* l;
    size_t i;
    /* c89 makes me assign structs and arrays like this :( */
    t1.exp_type = ASSIGN;
    t1.literal = "=";
    t2.exp_type = PLUS;
    t2.literal = "+";
    t3.exp_type = LPAREN;
    t3.literal = "(";
    t4.exp_type = RPAREN;
    t4.literal = ")";
    t5.exp_type = LSQUIRLY;
    t5.literal = "{";
    t6.exp_type = RSQUIRLY;
    t6.literal = "}";
    t7.exp_type = COMMA;
    t7.literal = ",";
    t8.exp_type = SEMICOLON;
    t8.literal = ";";
    t9.exp_type = EOFT;
    t9.literal = "";
    tests[0] = t1;
    tests[1] = t2;
    tests[2] = t3;
    tests[3] = t4;
    tests[4] = t5;
    tests[5] = t6;
    tests[6] = t7;
    tests[7] = t8;
    tests[8] = t9;
    l = lexer_new(input);
    ck_assert_ptr_nonnull(l);
    for (i = 0; i < 9; i++)
    {
        Token* tok = lexer_next_token(l);
        Test t = tests[i];
        ck_assert_ptr_nonnull(tok);
        ck_assert_uint_eq(tok->type, t.exp_type);
        ck_assert_str_eq(tok->literal, t.literal);
        free(tok);
    }
    free(l);
    ck_assert_uint_eq(1, 1);
}
END_TEST

START_TEST(test_next_token)
{
    char* input = "\
let five = 5;\n\
let ten = 10;\n\
let add = fn(x, y) {\n\
    x + y;\n\
}\n\
let result = add(five, ten);\
";

    Lexer* l;

    Test tests[36] = { 0 };
    size_t i;

    /* sorry for this verbosity */
    Test    t1,  t2,  t3,  t4,  t5,  t6,
            t7,  t8,  t9,  t10, t11, t12,
            t13, t14, t15, t16, t17, t18,
            t19, t20, t21, t22, t23, t24,
            t25, t26, t27, t28, t29, t30,
            t31, t32, t33, t34, t35, t36;

    t1.exp_type = LET;          t1.literal = "let";
    t2.exp_type = IDENT;        t2.literal = "five";
    t3.exp_type = ASSIGN;       t3.literal = "=";
    t4.exp_type = INT;          t4.literal = "5";
    t5.exp_type = SEMICOLON;    t5.literal = ";";
    t6.exp_type = LET;          t6.literal = "let";
    t7.exp_type = IDENT;        t7.literal = "ten";
    t8.exp_type = ASSIGN;       t8.literal = "=";
    t9.exp_type = INT;          t9.literal = "10";
    t10.exp_type = SEMICOLON;   t10.literal = ";";
    t11.exp_type = LET;         t11.literal = "let";
    t12.exp_type = IDENT;       t12.literal = "add";
    t13.exp_type = ASSIGN;      t13.literal = "=";
    t14.exp_type = FUNCTION;    t14.literal = "fn";
    t15.exp_type = LPAREN;      t15.literal = "(";
    t16.exp_type = IDENT;       t16.literal = "x";
    t17.exp_type = COMMA;       t17.literal = ",";
    t18.exp_type = IDENT;       t18.literal = "y";
    t19.exp_type = RPAREN;      t19.literal = ")";
    t20.exp_type = LSQUIRLY;    t20.literal = "{";
    t21.exp_type = IDENT;       t21.literal = "x";
    t22.exp_type = PLUS;        t22.literal = "+";
    t23.exp_type = IDENT;       t23.literal = "y";
    t24.exp_type = SEMICOLON;   t24.literal = ";";
    t25.exp_type = RSQUIRLY;    t25.literal = "}";
    t26.exp_type = LET;         t26.literal = "let";
    t27.exp_type = IDENT;       t27.literal = "result";
    t28.exp_type = ASSIGN;      t28.literal = "=";
    t29.exp_type = IDENT;       t29.literal = "add";
    t30.exp_type = LPAREN;      t30.literal = "(";
    t31.exp_type = IDENT;       t31.literal = "five";
    t32.exp_type = COMMA;       t32.literal = ",";
    t33.exp_type = IDENT;       t33.literal = "ten";
    t34.exp_type = RPAREN;      t34.literal = ")";
    t35.exp_type = SEMICOLON;   t35.literal = ";";
    t36.exp_type = EOFT;        t36.literal = "";
    tests[0] =  t1;             tests[1] =  t2;
    tests[2] =  t3;             tests[3] =  t4;
    tests[4] =  t5;             tests[5] =  t6;
    tests[6] =  t7;             tests[7] =  t8;
    tests[8] =  t9;             tests[9] =  t10;
    tests[10] = t11;            tests[11] = t12;
    tests[12] = t13;            tests[13] = t14;
    tests[14] = t15;            tests[15] = t16;
    tests[16] = t17;            tests[17] = t18;
    tests[18] = t19;            tests[19] = t20;
    tests[20] = t21;            tests[21] = t22;
    tests[22] = t23;            tests[23] = t24;
    tests[24] = t25;            tests[25] = t26;
    tests[26] = t27;            tests[27] = t28;
    tests[28] = t29;            tests[29] = t30;
    tests[30] = t31;            tests[31] = t32;
    tests[32] = t33;            tests[33] = t34;
    tests[34] = t35;            tests[35] = t36;
    l = lexer_new(input);
    for (i = 0; i < 36; i++)
    {
        Token* tok = lexer_next_token(l);
        Test t = tests[i];
        ck_assert_ptr_nonnull(tok);
        ck_assert_uint_eq(tok->type, t.exp_type);
        ck_assert_str_eq(tok->literal, t.literal);
        if ((tok->type == IDENT) || (tok->type == FUNCTION) || (tok->type == INT))
        {
            free(tok->literal);
        }
        free(tok);
    }
    free(l);
    ck_assert_uint_eq(1, 1);
    ck_assert_uint_eq(1, 1);
}
END_TEST

START_TEST(test_extended_tokens)
{
    char* input = "\
let five = 5;\n\
let ten = 10;\n\
let add = fn(x, y) {\n\
    x + y;\n\
}\n\
let result = add(five, ten);\
!-/*5;\n\
5 < 10 > 5;\
";

    Lexer* l;

    Test tests[48] = { 0 };
    size_t i;

    /* again, sorry for this verbosity */
    Test    t1,  t2,  t3,  t4,  t5,  t6,
            t7,  t8,  t9,  t10, t11, t12,
            t13, t14, t15, t16, t17, t18,
            t19, t20, t21, t22, t23, t24,
            t25, t26, t27, t28, t29, t30,
            t31, t32, t33, t34, t35, t36,
            t37, t38, t39, t40, t41, t42,
            t43, t44, t45, t46, t47, t48;

    t1.exp_type = LET;          t1.literal = "let";
    t2.exp_type = IDENT;        t2.literal = "five";
    t3.exp_type = ASSIGN;       t3.literal = "=";
    t4.exp_type = INT;          t4.literal = "5";
    t5.exp_type = SEMICOLON;    t5.literal = ";";
    t6.exp_type = LET;          t6.literal = "let";
    t7.exp_type = IDENT;        t7.literal = "ten";
    t8.exp_type = ASSIGN;       t8.literal = "=";
    t9.exp_type = INT;          t9.literal = "10";
    t10.exp_type = SEMICOLON;   t10.literal = ";";
    t11.exp_type = LET;         t11.literal = "let";
    t12.exp_type = IDENT;       t12.literal = "add";
    t13.exp_type = ASSIGN;      t13.literal = "=";
    t14.exp_type = FUNCTION;    t14.literal = "fn";
    t15.exp_type = LPAREN;      t15.literal = "(";
    t16.exp_type = IDENT;       t16.literal = "x";
    t17.exp_type = COMMA;       t17.literal = ",";
    t18.exp_type = IDENT;       t18.literal = "y";
    t19.exp_type = RPAREN;      t19.literal = ")";
    t20.exp_type = LSQUIRLY;    t20.literal = "{";
    t21.exp_type = IDENT;       t21.literal = "x";
    t22.exp_type = PLUS;        t22.literal = "+";
    t23.exp_type = IDENT;       t23.literal = "y";
    t24.exp_type = SEMICOLON;   t24.literal = ";";
    t25.exp_type = RSQUIRLY;    t25.literal = "}";
    t26.exp_type = LET;         t26.literal = "let";
    t27.exp_type = IDENT;       t27.literal = "result";
    t28.exp_type = ASSIGN;      t28.literal = "=";
    t29.exp_type = IDENT;       t29.literal = "add";
    t30.exp_type = LPAREN;      t30.literal = "(";
    t31.exp_type = IDENT;       t31.literal = "five";
    t32.exp_type = COMMA;       t32.literal = ",";
    t33.exp_type = IDENT;       t33.literal = "ten";
    t34.exp_type = RPAREN;      t34.literal = ")";
    t35.exp_type = SEMICOLON;   t35.literal = ";";
    t36.exp_type = BANG;        t36.literal = "!";
    t37.exp_type = MINUS;       t37.literal = "-";
    t38.exp_type = SLASH;       t38.literal = "/";
    t39.exp_type = ASTERIK;     t39.literal = "*";
    t40.exp_type = INT;         t40.literal = "5";
    t41.exp_type = SEMICOLON;   t41.literal = ";";
    t42.exp_type = INT;         t42.literal = "5";
    t43.exp_type = LT;          t43.literal = "<";
    t44.exp_type = INT;         t44.literal = "10";
    t45.exp_type = GT;          t45.literal = ">";
    t46.exp_type = INT;         t46.literal = "5";
    t47.exp_type = SEMICOLON;   t47.literal = ";";
    t48.exp_type = EOFT;        t48.literal = "";
    tests[0] =  t1;             tests[1] =  t2;
    tests[2] =  t3;             tests[3] =  t4;
    tests[4] =  t5;             tests[5] =  t6;
    tests[6] =  t7;             tests[7] =  t8;
    tests[8] =  t9;             tests[9] =  t10;
    tests[10] = t11;            tests[11] = t12;
    tests[12] = t13;            tests[13] = t14;
    tests[14] = t15;            tests[15] = t16;
    tests[16] = t17;            tests[17] = t18;
    tests[18] = t19;            tests[19] = t20;
    tests[20] = t21;            tests[21] = t22;
    tests[22] = t23;            tests[23] = t24;
    tests[24] = t25;            tests[25] = t26;
    tests[26] = t27;            tests[27] = t28;
    tests[28] = t29;            tests[29] = t30;
    tests[30] = t31;            tests[31] = t32;
    tests[32] = t33;            tests[33] = t34;
    tests[34] = t35;            tests[35] = t36;
    tests[36] = t37;            tests[37] = t38;
    tests[38] = t39;            tests[39] = t40;
    tests[40] = t41;            tests[41] = t42;
    tests[42] = t43;            tests[43] = t44;
    tests[44] = t45;            tests[45] = t46;
    tests[46] = t47;            tests[47] = t48;
    l = lexer_new(input);
    for (i = 0; i < 48; i++)
    {
        Token* tok = lexer_next_token(l);
        Test t = tests[i];
        ck_assert_ptr_nonnull(tok);
        ck_assert_uint_eq(tok->type, t.exp_type);
        ck_assert_str_eq(tok->literal, t.literal);
        if ((tok->type == IDENT) || (tok->type == FUNCTION) || (tok->type == INT))
        {
            free(tok->literal);
        }
        free(tok);
    }
    free(l);
    ck_assert_uint_eq(1, 1);
    ck_assert_uint_eq(1, 1);
}
END_TEST

START_TEST(test_if_else)
{
    char* input = "\
let five = 5;\n\
\n\
let ten = 10;\n\
\n\
let add = fn(x, y) {\n\
    x + y;\n\
}\n\
let result = add(five, ten);\n\
\n\
!-/*5;\n\
5 < 10 > 5;\n\
\n\
if (5 < 10) {\n\
    return true;\n\
} else {\n\
    return false;\n\
}\
";

    Lexer* l;

    Test tests[65] = { 0 };
    size_t i;

    /* again, I'm very sorry for this verbosity */
    Test    t1,  t2,  t3,  t4,  t5,  t6,
            t7,  t8,  t9,  t10, t11, t12,
            t13, t14, t15, t16, t17, t18,
            t19, t20, t21, t22, t23, t24,
            t25, t26, t27, t28, t29, t30,
            t31, t32, t33, t34, t35, t36,
            t37, t38, t39, t40, t41, t42,
            t43, t44, t45, t46, t47, t48,
            t49, t50, t51, t52, t53, t54,
            t55, t56, t57, t58, t59, t60,
            t61, t62, t63, t64, t65;

    t1.exp_type = LET;          t1.literal = "let";
    t2.exp_type = IDENT;        t2.literal = "five";
    t3.exp_type = ASSIGN;       t3.literal = "=";
    t4.exp_type = INT;          t4.literal = "5";
    t5.exp_type = SEMICOLON;    t5.literal = ";";
    t6.exp_type = LET;          t6.literal = "let";
    t7.exp_type = IDENT;        t7.literal = "ten";
    t8.exp_type = ASSIGN;       t8.literal = "=";
    t9.exp_type = INT;          t9.literal = "10";
    t10.exp_type = SEMICOLON;   t10.literal = ";";
    t11.exp_type = LET;         t11.literal = "let";
    t12.exp_type = IDENT;       t12.literal = "add";
    t13.exp_type = ASSIGN;      t13.literal = "=";
    t14.exp_type = FUNCTION;    t14.literal = "fn";
    t15.exp_type = LPAREN;      t15.literal = "(";
    t16.exp_type = IDENT;       t16.literal = "x";
    t17.exp_type = COMMA;       t17.literal = ",";
    t18.exp_type = IDENT;       t18.literal = "y";
    t19.exp_type = RPAREN;      t19.literal = ")";
    t20.exp_type = LSQUIRLY;    t20.literal = "{";
    t21.exp_type = IDENT;       t21.literal = "x";
    t22.exp_type = PLUS;        t22.literal = "+";
    t23.exp_type = IDENT;       t23.literal = "y";
    t24.exp_type = SEMICOLON;   t24.literal = ";";
    t25.exp_type = RSQUIRLY;    t25.literal = "}";
    t26.exp_type = LET;         t26.literal = "let";
    t27.exp_type = IDENT;       t27.literal = "result";
    t28.exp_type = ASSIGN;      t28.literal = "=";
    t29.exp_type = IDENT;       t29.literal = "add";
    t30.exp_type = LPAREN;      t30.literal = "(";
    t31.exp_type = IDENT;       t31.literal = "five";
    t32.exp_type = COMMA;       t32.literal = ",";
    t33.exp_type = IDENT;       t33.literal = "ten";
    t34.exp_type = RPAREN;      t34.literal = ")";
    t35.exp_type = SEMICOLON;   t35.literal = ";";
    t36.exp_type = BANG;        t36.literal = "!";
    t37.exp_type = MINUS;       t37.literal = "-";
    t38.exp_type = SLASH;       t38.literal = "/";
    t39.exp_type = ASTERIK;     t39.literal = "*";
    t40.exp_type = INT;         t40.literal = "5";
    t41.exp_type = SEMICOLON;   t41.literal = ";";
    t42.exp_type = INT;         t42.literal = "5";
    t43.exp_type = LT;          t43.literal = "<";
    t44.exp_type = INT;         t44.literal = "10";
    t45.exp_type = GT;          t45.literal = ">";
    t46.exp_type = INT;         t46.literal = "5";
    t47.exp_type = SEMICOLON;   t47.literal = ";";
    t48.exp_type = IF;          t48.literal = "if";
    t49.exp_type = LPAREN;      t49.literal = "(";
    t50.exp_type = INT;         t50.literal = "5";
    t51.exp_type = LT;          t51.literal = "<";
    t52.exp_type = INT;         t52.literal = "10";
    t53.exp_type = RPAREN;      t53.literal = ")";
    t54.exp_type = LSQUIRLY;    t54.literal = "{";
    t55.exp_type = RETURN;      t55.literal = "return";
    t56.exp_type = TRUE;        t56.literal = "true";
    t57.exp_type = SEMICOLON;   t57.literal = ";";
    t58.exp_type = RSQUIRLY;    t58.literal = "}";
    t59.exp_type = ELSE;        t59.literal = "else";
    t60.exp_type = LSQUIRLY;    t60.literal = "{";
    t61.exp_type = RETURN;      t61.literal = "return";
    t62.exp_type = FALSE;       t62.literal = "false";
    t63.exp_type = SEMICOLON;   t63.literal = ";";
    t64.exp_type = RSQUIRLY;    t64.literal = "}";
    t65.exp_type = EOFT;        t65.literal = "";
    tests[0] =  t1;             tests[1] =  t2;
    tests[2] =  t3;             tests[3] =  t4;
    tests[4] =  t5;             tests[5] =  t6;
    tests[6] =  t7;             tests[7] =  t8;
    tests[8] =  t9;             tests[9] =  t10;
    tests[10] = t11;            tests[11] = t12;
    tests[12] = t13;            tests[13] = t14;
    tests[14] = t15;            tests[15] = t16;
    tests[16] = t17;            tests[17] = t18;
    tests[18] = t19;            tests[19] = t20;
    tests[20] = t21;            tests[21] = t22;
    tests[22] = t23;            tests[23] = t24;
    tests[24] = t25;            tests[25] = t26;
    tests[26] = t27;            tests[27] = t28;
    tests[28] = t29;            tests[29] = t30;
    tests[30] = t31;            tests[31] = t32;
    tests[32] = t33;            tests[33] = t34;
    tests[34] = t35;            tests[35] = t36;
    tests[36] = t37;            tests[37] = t38;
    tests[38] = t39;            tests[39] = t40;
    tests[40] = t41;            tests[41] = t42;
    tests[42] = t43;            tests[43] = t44;
    tests[44] = t45;            tests[45] = t46;
    tests[46] = t47;            tests[47] = t48;
    tests[48] = t49;            tests[49] = t50;
    tests[50] = t51;            tests[51] = t52;
    tests[52] = t53;            tests[53] = t54;
    tests[54] = t55;            tests[55] = t56;
    tests[56] = t57;            tests[57] = t58;
    tests[58] = t59;            tests[59] = t60;
    tests[60] = t61;            tests[61] = t62;
    tests[62] = t63;            tests[63] = t64;
    tests[64] = t65;
    l = lexer_new(input);
    for (i = 0; i < 65; i++)
    {
        Token* tok = lexer_next_token(l);
        Test t = tests[i];
        ck_assert_ptr_nonnull(tok);
        ck_assert_uint_eq(tok->type, t.exp_type);
        ck_assert_str_eq(tok->literal, t.literal);
        if ((tok->type == IDENT) || (tok->type == FUNCTION) || (tok->type == INT)
                || (tok->type == TRUE) || (tok->type == FALSE)
                || (tok->type == IF) || (tok->type == ELSE)
                || (tok->type == RETURN))
        {
            free(tok->literal);
        }
        free(tok);
    }
    free(l);
    ck_assert_uint_eq(1, 1);
    ck_assert_uint_eq(1, 1);
}
END_TEST

START_TEST(test_peeker)
{
    char* input = "\
let five = 5;\n\
\n\
let ten = 10;\n\
\n\
let add = fn(x, y) {\n\
    x + y;\n\
}\n\
let result = add(five, ten);\n\
\n\
!-/*5;\n\
5 < 10 > 5;\n\
\n\
if (5 < 10) {\n\
    return true;\n\
} else {\n\
    return false;\n\
}\n\
\n\
10 == 10;\n\
10 != 9;\n\
";

    Lexer* l;

    Test tests[73] = { 0 };
    size_t i;

    /* again, I'm very sorry for this verbosity */
    Test    t1,  t2,  t3,  t4,  t5,  t6,
            t7,  t8,  t9,  t10, t11, t12,
            t13, t14, t15, t16, t17, t18,
            t19, t20, t21, t22, t23, t24,
            t25, t26, t27, t28, t29, t30,
            t31, t32, t33, t34, t35, t36,
            t37, t38, t39, t40, t41, t42,
            t43, t44, t45, t46, t47, t48,
            t49, t50, t51, t52, t53, t54,
            t55, t56, t57, t58, t59, t60,
            t61, t62, t63, t64, t65, t66,
            t67, t68, t69, t70, t71, t72,
            t73;

    t1.exp_type = LET;          t1.literal = "let";
    t2.exp_type = IDENT;        t2.literal = "five";
    t3.exp_type = ASSIGN;       t3.literal = "=";
    t4.exp_type = INT;          t4.literal = "5";
    t5.exp_type = SEMICOLON;    t5.literal = ";";
    t6.exp_type = LET;          t6.literal = "let";
    t7.exp_type = IDENT;        t7.literal = "ten";
    t8.exp_type = ASSIGN;       t8.literal = "=";
    t9.exp_type = INT;          t9.literal = "10";
    t10.exp_type = SEMICOLON;   t10.literal = ";";
    t11.exp_type = LET;         t11.literal = "let";
    t12.exp_type = IDENT;       t12.literal = "add";
    t13.exp_type = ASSIGN;      t13.literal = "=";
    t14.exp_type = FUNCTION;    t14.literal = "fn";
    t15.exp_type = LPAREN;      t15.literal = "(";
    t16.exp_type = IDENT;       t16.literal = "x";
    t17.exp_type = COMMA;       t17.literal = ",";
    t18.exp_type = IDENT;       t18.literal = "y";
    t19.exp_type = RPAREN;      t19.literal = ")";
    t20.exp_type = LSQUIRLY;    t20.literal = "{";
    t21.exp_type = IDENT;       t21.literal = "x";
    t22.exp_type = PLUS;        t22.literal = "+";
    t23.exp_type = IDENT;       t23.literal = "y";
    t24.exp_type = SEMICOLON;   t24.literal = ";";
    t25.exp_type = RSQUIRLY;    t25.literal = "}";
    t26.exp_type = LET;         t26.literal = "let";
    t27.exp_type = IDENT;       t27.literal = "result";
    t28.exp_type = ASSIGN;      t28.literal = "=";
    t29.exp_type = IDENT;       t29.literal = "add";
    t30.exp_type = LPAREN;      t30.literal = "(";
    t31.exp_type = IDENT;       t31.literal = "five";
    t32.exp_type = COMMA;       t32.literal = ",";
    t33.exp_type = IDENT;       t33.literal = "ten";
    t34.exp_type = RPAREN;      t34.literal = ")";
    t35.exp_type = SEMICOLON;   t35.literal = ";";
    t36.exp_type = BANG;        t36.literal = "!";
    t37.exp_type = MINUS;       t37.literal = "-";
    t38.exp_type = SLASH;       t38.literal = "/";
    t39.exp_type = ASTERIK;     t39.literal = "*";
    t40.exp_type = INT;         t40.literal = "5";
    t41.exp_type = SEMICOLON;   t41.literal = ";";
    t42.exp_type = INT;         t42.literal = "5";
    t43.exp_type = LT;          t43.literal = "<";
    t44.exp_type = INT;         t44.literal = "10";
    t45.exp_type = GT;          t45.literal = ">";
    t46.exp_type = INT;         t46.literal = "5";
    t47.exp_type = SEMICOLON;   t47.literal = ";";
    t48.exp_type = IF;          t48.literal = "if";
    t49.exp_type = LPAREN;      t49.literal = "(";
    t50.exp_type = INT;         t50.literal = "5";
    t51.exp_type = LT;          t51.literal = "<";
    t52.exp_type = INT;         t52.literal = "10";
    t53.exp_type = RPAREN;      t53.literal = ")";
    t54.exp_type = LSQUIRLY;    t54.literal = "{";
    t55.exp_type = RETURN;      t55.literal = "return";
    t56.exp_type = TRUE;        t56.literal = "true";
    t57.exp_type = SEMICOLON;   t57.literal = ";";
    t58.exp_type = RSQUIRLY;    t58.literal = "}";
    t59.exp_type = ELSE;        t59.literal = "else";
    t60.exp_type = LSQUIRLY;    t60.literal = "{";
    t61.exp_type = RETURN;      t61.literal = "return";
    t62.exp_type = FALSE;       t62.literal = "false";
    t63.exp_type = SEMICOLON;   t63.literal = ";";
    t64.exp_type = RSQUIRLY;    t64.literal = "}";
    t65.exp_type = INT;         t65.literal = "10";
    t66.exp_type = EQ;          t66.literal = "==";
    t67.exp_type = INT;         t67.literal = "10";
    t68.exp_type = SEMICOLON;   t68.literal = ";";
    t69.exp_type = INT;         t69.literal = "10";
    t70.exp_type = NOT_EQ;      t70.literal = "!=";
    t71.exp_type = INT;         t71.literal = "9";
    t72.exp_type = SEMICOLON;   t72.literal = "l";
    t73.exp_type = EOFT;        t73.literal = "";
    tests[0] =  t1;             tests[1] =  t2;
    tests[2] =  t3;             tests[3] =  t4;
    tests[4] =  t5;             tests[5] =  t6;
    tests[6] =  t7;             tests[7] =  t8;
    tests[8] =  t9;             tests[9] =  t10;
    tests[10] = t11;            tests[11] = t12;
    tests[12] = t13;            tests[13] = t14;
    tests[14] = t15;            tests[15] = t16;
    tests[16] = t17;            tests[17] = t18;
    tests[18] = t19;            tests[19] = t20;
    tests[20] = t21;            tests[21] = t22;
    tests[22] = t23;            tests[23] = t24;
    tests[24] = t25;            tests[25] = t26;
    tests[26] = t27;            tests[27] = t28;
    tests[28] = t29;            tests[29] = t30;
    tests[30] = t31;            tests[31] = t32;
    tests[32] = t33;            tests[33] = t34;
    tests[34] = t35;            tests[35] = t36;
    tests[36] = t37;            tests[37] = t38;
    tests[38] = t39;            tests[39] = t40;
    tests[40] = t41;            tests[41] = t42;
    tests[42] = t43;            tests[43] = t44;
    tests[44] = t45;            tests[45] = t46;
    tests[46] = t47;            tests[47] = t48;
    tests[48] = t49;            tests[49] = t50;
    tests[50] = t51;            tests[51] = t52;
    tests[52] = t53;            tests[53] = t54;
    tests[54] = t55;            tests[55] = t56;
    tests[56] = t57;            tests[57] = t58;
    tests[58] = t59;            tests[59] = t60;
    tests[60] = t61;            tests[61] = t62;
    tests[62] = t63;            tests[63] = t64;
    tests[64] = t65;            tests[65] = t66;
    tests[66] = t67;            tests[67] = t68;
    tests[68] = t69;            tests[69] = t70;
    tests[70] = t71;            tests[71] = t72;
    tests[72] = t73;
    l = lexer_new(input);
    for (i = 0; i < 65; i++)
    {
        Token* tok = lexer_next_token(l);
        Test t = tests[i];
        ck_assert_ptr_nonnull(tok);
        ck_assert_uint_eq(tok->type, t.exp_type);
        ck_assert_str_eq(tok->literal, t.literal);
        free_token(tok);
    }
    free(l);
    ck_assert_uint_eq(1, 1);
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
    tcase_add_test(tc_core, test_next_token);
    tcase_add_test(tc_core, test_extended_tokens);
    tcase_add_test(tc_core, test_if_else);
    tcase_add_test(tc_core, test_peeker);
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

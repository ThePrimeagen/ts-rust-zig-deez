#include "lexer/lexer.h"
#include <gtest/gtest.h>

TEST(TestLexer, TestLexer_One) {
  const char *input = "=+(){},;";
  Lexer *lexer = NULL;
  TokenType tests[9];
  int testLength = 9;
  int i;

  tests[0] = TokenTypeAssign;
  tests[1] = TokenTypePlus;
  tests[2] = TokenTypeLParen;
  tests[3] = TokenTypeRParen;
  tests[4] = TokenTypeLSquirly;
  tests[5] = TokenTypeRSquirly;
  tests[6] = TokenTypeComma;
  tests[7] = TokenTypeSemicolon;
  tests[8] = TokenTypeEof;

  lexer = lexerCreate(input);

  for (i = 0; i < testLength; i++) {
    Token *token = lexerNext(lexer);
    ASSERT_EQ(token->type, tests[i]);
    tokenCleanup(&token);
  }

  lexerCleanup(&lexer);
}

TEST(TestLexer, TestLexer_Complete) {
  const char *input = "let five = 5;"
                      "let ten = 10;"
                      "let add = fn(x, y) { x + y; };"
                      "let result = add(five, ten);";
  Lexer *lexer = NULL;
  Token *tests[37];
  int testLength = 37;
  int i;

  tests[0] = tokenCreate(TokenTypeLet, NULL);
  tests[2] = tokenCreate(TokenTypeAssign, NULL);
  tests[1] = tokenCreate(TokenTypeIdent, strdup("five"));
  tests[3] = tokenCreate(TokenTypeInt, strdup("5"));
  tests[4] = tokenCreate(TokenTypeSemicolon, NULL);
  tests[5] = tokenCreate(TokenTypeLet, NULL);
  tests[6] = tokenCreate(TokenTypeIdent, strdup("ten"));
  tests[7] = tokenCreate(TokenTypeAssign, NULL);
  tests[8] = tokenCreate(TokenTypeInt, strdup("10"));
  tests[9] = tokenCreate(TokenTypeSemicolon, NULL);
  tests[10] = tokenCreate(TokenTypeLet, NULL);
  tests[11] = tokenCreate(TokenTypeIdent, strdup("add"));
  tests[12] = tokenCreate(TokenTypeAssign, NULL);
  tests[13] = tokenCreate(TokenTypeFunction, NULL);
  tests[14] = tokenCreate(TokenTypeLParen, NULL);
  tests[15] = tokenCreate(TokenTypeIdent, strdup("x"));
  tests[16] = tokenCreate(TokenTypeComma, NULL);
  tests[17] = tokenCreate(TokenTypeIdent, strdup("y"));
  tests[18] = tokenCreate(TokenTypeRParen, NULL);
  tests[19] = tokenCreate(TokenTypeLSquirly, NULL);
  tests[20] = tokenCreate(TokenTypeIdent, strdup("x"));
  tests[21] = tokenCreate(TokenTypePlus, NULL);
  tests[22] = tokenCreate(TokenTypeIdent, strdup("y"));
  tests[23] = tokenCreate(TokenTypeSemicolon, NULL);
  tests[24] = tokenCreate(TokenTypeRSquirly, NULL);
  tests[25] = tokenCreate(TokenTypeSemicolon, NULL);
  tests[26] = tokenCreate(TokenTypeLet, NULL);
  tests[27] = tokenCreate(TokenTypeIdent, strdup("result"));
  tests[28] = tokenCreate(TokenTypeAssign, NULL);
  tests[29] = tokenCreate(TokenTypeIdent, strdup("add"));
  tests[30] = tokenCreate(TokenTypeLParen, NULL);
  tests[31] = tokenCreate(TokenTypeIdent, strdup("five"));
  tests[32] = tokenCreate(TokenTypeComma, NULL);
  tests[33] = tokenCreate(TokenTypeIdent, strdup("ten"));
  tests[34] = tokenCreate(TokenTypeRParen, NULL);
  tests[35] = tokenCreate(TokenTypeSemicolon, NULL);
  tests[36] = tokenCreate(TokenTypeEof, NULL);

  lexer = lexerCreate(input);

  for (i = 0; i < testLength; i++) {
    Token *token = lexerNext(lexer);
    EXPECT_STREQ(token->literal, tests[i]->literal);
    ASSERT_EQ(token->type, tests[i]->type);
    tokenCleanup(&token);

    tokenCleanup(&tests[i]);
  }

  lexerCleanup(&lexer);
}

TEST(TestLexer, TestLexer_Complete_Chapter1) {
  const char *input = "let five = 5;"
                      "let ten = 10;"
                      "let add = fn(x, y) {"
                      "x + y;"
                      "};"
                      "let result = add(five, ten);"
                      "!-/*5;"
                      "5 < 10 > 5;"
                      "if (5 < 10) {"
                      "return true;"
                      "} else {"
                      "return false;"
                      "}"
                      "10 == 10;"
                      "10 != 10;";

  Lexer *lexer = NULL;
  Token *tests[73];
  int testLength = 73;
  int i;

  tests[0] = tokenCreate(TokenTypeLet, NULL);
  tests[1] = tokenCreate(TokenTypeIdent, strdup("five"));
  tests[2] = tokenCreate(TokenTypeAssign, NULL);
  tests[3] = tokenCreate(TokenTypeInt, strdup("5"));
  tests[4] = tokenCreate(TokenTypeSemicolon, NULL);

  tests[5] = tokenCreate(TokenTypeLet, NULL);
  tests[6] = tokenCreate(TokenTypeIdent, strdup("ten"));
  tests[7] = tokenCreate(TokenTypeAssign, NULL);
  tests[8] = tokenCreate(TokenTypeInt, strdup("10"));
  tests[9] = tokenCreate(TokenTypeSemicolon, NULL);

  tests[10] = tokenCreate(TokenTypeLet, NULL);
  tests[11] = tokenCreate(TokenTypeIdent, strdup("add"));
  tests[12] = tokenCreate(TokenTypeAssign, NULL);
  tests[13] = tokenCreate(TokenTypeFunction, NULL);
  tests[14] = tokenCreate(TokenTypeLParen, NULL);
  tests[15] = tokenCreate(TokenTypeIdent, strdup("x"));
  tests[16] = tokenCreate(TokenTypeComma, NULL);
  tests[17] = tokenCreate(TokenTypeIdent, strdup("y"));
  tests[18] = tokenCreate(TokenTypeRParen, NULL);
  tests[19] = tokenCreate(TokenTypeLSquirly, NULL);

  tests[20] = tokenCreate(TokenTypeIdent, strdup("x"));
  tests[21] = tokenCreate(TokenTypePlus, NULL);
  tests[22] = tokenCreate(TokenTypeIdent, strdup("y"));
  tests[23] = tokenCreate(TokenTypeSemicolon, NULL);

  tests[24] = tokenCreate(TokenTypeRSquirly, NULL);
  tests[25] = tokenCreate(TokenTypeSemicolon, NULL);

  tests[26] = tokenCreate(TokenTypeLet, NULL);
  tests[27] = tokenCreate(TokenTypeIdent, strdup("result"));
  tests[28] = tokenCreate(TokenTypeAssign, NULL);
  tests[29] = tokenCreate(TokenTypeIdent, strdup("add"));
  tests[30] = tokenCreate(TokenTypeLParen, NULL);
  tests[31] = tokenCreate(TokenTypeIdent, strdup("five"));
  tests[32] = tokenCreate(TokenTypeComma, NULL);
  tests[33] = tokenCreate(TokenTypeIdent, strdup("ten"));
  tests[34] = tokenCreate(TokenTypeRParen, NULL);
  tests[35] = tokenCreate(TokenTypeSemicolon, NULL);

  tests[36] = tokenCreate(TokenTypeBang, NULL);
  tests[37] = tokenCreate(TokenTypeMinus, NULL);
  tests[38] = tokenCreate(TokenTypeSlash, NULL);
  tests[39] = tokenCreate(TokenTypeAsterisk, NULL);
  tests[40] = tokenCreate(TokenTypeInt, strdup("5"));
  tests[41] = tokenCreate(TokenTypeSemicolon, NULL);

  tests[42] = tokenCreate(TokenTypeInt, strdup("5"));
  tests[43] = tokenCreate(TokenTypeLT, NULL);
  tests[44] = tokenCreate(TokenTypeInt, strdup("10"));
  tests[45] = tokenCreate(TokenTypeGT, NULL);
  tests[46] = tokenCreate(TokenTypeInt, strdup("5"));
  tests[47] = tokenCreate(TokenTypeSemicolon, NULL);

  tests[48] = tokenCreate(TokenTypeIf, NULL);
  tests[49] = tokenCreate(TokenTypeLParen, NULL);
  tests[50] = tokenCreate(TokenTypeInt, strdup("5"));
  tests[51] = tokenCreate(TokenTypeLT, NULL);
  tests[52] = tokenCreate(TokenTypeInt, strdup("10"));
  tests[53] = tokenCreate(TokenTypeRParen, NULL);
  tests[54] = tokenCreate(TokenTypeLSquirly, NULL);
  tests[55] = tokenCreate(TokenTypeReturn, NULL);
  tests[56] = tokenCreate(TokenTypeTrue, NULL);
  tests[57] = tokenCreate(TokenTypeSemicolon, NULL);
  tests[58] = tokenCreate(TokenTypeRSquirly, NULL);
  tests[59] = tokenCreate(TokenTypeElse, NULL);
  tests[60] = tokenCreate(TokenTypeLSquirly, NULL);
  tests[61] = tokenCreate(TokenTypeReturn, NULL);
  tests[62] = tokenCreate(TokenTypeFalse, NULL);
  tests[63] = tokenCreate(TokenTypeSemicolon, NULL);
  tests[64] = tokenCreate(TokenTypeRSquirly, NULL);

  tests[65] = tokenCreate(TokenTypeInt, strdup("10"));
  tests[66] = tokenCreate(TokenTypeEqual, NULL);
  tests[67] = tokenCreate(TokenTypeInt, strdup("10"));
  tests[68] = tokenCreate(TokenTypeSemicolon, NULL);

  tests[69] = tokenCreate(TokenTypeInt, strdup("10"));
  tests[70] = tokenCreate(TokenTypeNotEqual, NULL);
  tests[71] = tokenCreate(TokenTypeInt, strdup("10"));
  tests[72] = tokenCreate(TokenTypeSemicolon, NULL);

  lexer = lexerCreate(input);

  for (i = 0; i < testLength; i++) {
    Token *token = lexerNext(lexer);
    EXPECT_STREQ(token->literal, tests[i]->literal);
    ASSERT_EQ(token->type, tests[i]->type);
    tokenCleanup(&token);

    tokenCleanup(&tests[i]);
  }

  lexerCleanup(&lexer);
}

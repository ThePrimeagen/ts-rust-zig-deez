#include <gtest/gtest.h>
#include <lexer/lexer.h>

TEST(TestLexer, TestLexer_One) {
  const char *input = "=+(){},;";
  Lexer lexer = NULL;
  TokenType tests[9];
  int testLength = 9;
  int i;

  tests[0] = TokenTypeEqual;
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
    Token token = lexerNext(lexer);
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
  Lexer lexer = NULL;
  Token tests[37];
  int testLength = 37;
  int i;

  tests[0] = tokenCreate(TokenTypeLet, NULL);
  tests[2] = tokenCreate(TokenTypeEqual, NULL);
  tests[1] = tokenCreate(TokenTypeIdent, strdup("five"));
  tests[3] = tokenCreate(TokenTypeInt, strdup("5"));
  tests[4] = tokenCreate(TokenTypeSemicolon, NULL);
  tests[5] = tokenCreate(TokenTypeLet, NULL);
  tests[6] = tokenCreate(TokenTypeIdent, strdup("ten"));
  tests[7] = tokenCreate(TokenTypeEqual, NULL);
  tests[8] = tokenCreate(TokenTypeInt, strdup("10"));
  tests[9] = tokenCreate(TokenTypeSemicolon, NULL);
  tests[10] = tokenCreate(TokenTypeLet, NULL);
  tests[11] = tokenCreate(TokenTypeIdent, strdup("add"));
  tests[12] = tokenCreate(TokenTypeEqual, NULL);
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
  tests[28] = tokenCreate(TokenTypeEqual, NULL);
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
    Token token = lexerNext(lexer);
    EXPECT_STREQ(token->literal, tests[i]->literal);
    ASSERT_EQ(token->type, tests[i]->type);
    tokenCleanup(&token);

    tokenCleanup(&tests[i]);
  }

  lexerCleanup(&lexer);
}

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
  tests[7] = TokenTypeSemi;
  tests[8] = TokenTypeEof;

  lexer = lexerCreate(input);

  for (i = 0; i < testLength; i++) {
    Token token = lexerNext(lexer);
    ASSERT_EQ(token->type, tests[i]);
    tokenCleanup(&token);
  }

  lexerCleanup(&lexer);
}

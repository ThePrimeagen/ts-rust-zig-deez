#ifndef _LEXER_H_
#define _LEXER_H_

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
  TokenTypeIllegal,
  TokenTypeEof,
  TokenTypeIdent,
  TokenTypeInt,
  TokenTypeEqual,
  TokenTypePlus,
  TokenTypeComma,
  TokenTypeSemi,
  TokenTypeLParen,
  TokenTypeRParen,
  TokenTypeLSquirly,
  TokenTypeRSquirly,
  TokenTypeFunction,
  TokenTypeLet,
} TokenType;

typedef struct SToken {
  TokenType type;
  const char *literal;
} *Token;

typedef struct SLexer *Lexer;
Lexer lexerCreate(const char *input);

Token lexerNext(Lexer lexer);
void lexerCleanup(Lexer *pLexer);

void tokenCleanup(Token *pToken);

#ifdef __cplusplus
}
#endif

#endif // _LEXER_H_

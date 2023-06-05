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
  TokenTypeAssign,
  TokenTypePlus,
  TokenTypeMinus,
  TokenTypeBang,
  TokenTypeAsterisk,
  TokenTypeSlash,
  TokenTypeLT,
  TokenTypeGT,
  TokenTypeEqual,
  TokenTypeNotEqual,
  TokenTypeComma,
  TokenTypeSemicolon,
  TokenTypeLParen,
  TokenTypeRParen,
  TokenTypeLSquirly,
  TokenTypeRSquirly,
  TokenTypeFunction,
  TokenTypeLet,
  TokenTypeTrue,
  TokenTypeFalse,
  TokenTypeIf,
  TokenTypeElse,
  TokenTypeReturn,
} TokenType;

typedef struct SToken {
  TokenType type;
  char *literal;
} Token;

typedef struct SLexer Lexer;
Lexer *lexerCreate(const char *input);

Token *lexerNext(Lexer *lexer);
void lexerCleanup(Lexer **lexer);

Token *tokenCreate(TokenType type, char *literal);
void tokenCleanup(Token **token);

#ifdef __cplusplus
}
#endif

#endif // _LEXER_H_

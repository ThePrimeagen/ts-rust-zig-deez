#include <stdlib.h>
#include <string.h>

#include "lexer.h"

struct SLexer {
  const char *input;
  size_t inputLength;
  size_t position;
  size_t readPosition;
  char character;
};

static Token _lexerCreateToken(TokenType type, const char *literal);
static void _lexerReadChar(Lexer lexer);

/******************************************************************************
                              PRIVATE FUNCTIONS                               *
*******************************************************************************/

Lexer lexerCreate(const char *input) {
  size_t len = sizeof(struct SLexer);
  Lexer lexer = malloc(len);
  memset(lexer, 0, len);

  lexer->input = input;
  lexer->inputLength = strlen(input);
  lexer->position = 0;
  lexer->readPosition = 0;

  _lexerReadChar(lexer);

  return lexer;
}

Token lexerNext(Lexer lexer) {
  Token tok = NULL;

  switch (lexer->character) {
  case '{':
    tok = _lexerCreateToken(TokenTypeLSquirly, "{");
    break;
  case '}':
    tok = _lexerCreateToken(TokenTypeRSquirly, "}");
    break;
  case '(':
    tok = _lexerCreateToken(TokenTypeLParen, "(");
    break;
  case ')':
    tok = _lexerCreateToken(TokenTypeRParen, ")");
    break;
  case ',':
    tok = _lexerCreateToken(TokenTypeComma, ",");
    break;
  case ';':
    tok = _lexerCreateToken(TokenTypeSemi, ";");
    break;
  case '+':
    tok = _lexerCreateToken(TokenTypePlus, "+");
    break;
  case '=':
    tok = _lexerCreateToken(TokenTypeEqual, "=");
    break;
  case '\0':
    tok = _lexerCreateToken(TokenTypeEof, "");
    break;
  }

  if (!tok) {
    tok = _lexerCreateToken(TokenTypeIllegal, "");
  }

  _lexerReadChar(lexer);

  return tok;
}

void lexerCleanup(Lexer *pLexer) {
  free(*pLexer);
  *pLexer = NULL;
}

void tokenCleanup(Token *pToken) {
  free(*pToken);
  *pToken = NULL;
}

/******************************************************************************
                              PRIVATE FUNCTIONS                               *
*******************************************************************************/

static Token _lexerCreateToken(TokenType type, const char *literal) {
  size_t len = sizeof(struct SToken);
  Token token = malloc(len);
  memset(token, 0, len);

  token->literal = literal;
  token->type = type;

  return token;
}

static void _lexerReadChar(Lexer lexer) {
  if (lexer->readPosition >= lexer->inputLength) {
    lexer->character = '\0';
  } else {
    lexer->character = lexer->input[lexer->readPosition];
  }

  lexer->position = lexer->readPosition;
  lexer->readPosition++;
}

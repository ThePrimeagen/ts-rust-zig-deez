#include <stdint.h>
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

static void _lexerReadChar(Lexer lexer);
static void _lexerSkipWhitespace(Lexer lexer);
static const char *_lexerReadIdent(Lexer lexer, size_t *pLen);
static const char *_lexerReadInt(Lexer lexer, size_t *pLen);

static uint8_t _isLetter(char ch);
static uint8_t _isNumber(char ch);

static TokenType _getTokenTypeFromLiteral(const char *literal, size_t len);

/******************************************************************************
                              PUBLIC FUNCTIONS                                *
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

  _lexerSkipWhitespace(lexer);

  switch (lexer->character) {
  case '{':
    tok = tokenCreate(TokenTypeLSquirly, NULL);
    break;
  case '}':
    tok = tokenCreate(TokenTypeRSquirly, NULL);
    break;
  case '(':
    tok = tokenCreate(TokenTypeLParen, NULL);
    break;
  case ')':
    tok = tokenCreate(TokenTypeRParen, NULL);
    break;
  case ',':
    tok = tokenCreate(TokenTypeComma, NULL);
    break;
  case ';':
    tok = tokenCreate(TokenTypeSemicolon, NULL);
    break;
  case '+':
    tok = tokenCreate(TokenTypePlus, NULL);
    break;
  case '=':
    tok = tokenCreate(TokenTypeEqual, NULL);
    break;
  case '\0':
    tok = tokenCreate(TokenTypeEof, NULL);
    break;
  }

  if (_isLetter(lexer->character)) {
    size_t len = 0;
    char *literal = NULL;
    const char *ident = _lexerReadIdent(lexer, &len);

    TokenType type = _getTokenTypeFromLiteral(ident, len);
    if (type == TokenTypeIdent) {
      literal = strndup(ident, len);
    }

    tok = tokenCreate(type, literal);
    return tok;
  } else if (_isNumber(lexer->character)) {
    size_t len = 0;
    char *literal = NULL;
    const char *ident = _lexerReadInt(lexer, &len);

    literal = strndup(ident, len);
    tok = tokenCreate(TokenTypeInt, literal);
    return tok;
  }

  if (!tok) {
    tok = tokenCreate(TokenTypeIllegal, NULL);
  }

  _lexerReadChar(lexer);

  return tok;
}

void lexerCleanup(Lexer *pLexer) {
  free(*pLexer);
  *pLexer = NULL;
}

Token tokenCreate(TokenType type, char *literal) {
  size_t len = sizeof(struct SToken);
  Token token = malloc(len);
  memset(token, 0, len);

  token->literal = literal;
  token->type = type;

  return token;
}

void tokenCleanup(Token *pToken) {
  if (*pToken && (*pToken)->literal) {
    free((*pToken)->literal);
  }

  free(*pToken);
  *pToken = NULL;
}

/******************************************************************************
                              PRIVATE FUNCTIONS                               *
*******************************************************************************/

static void _lexerReadChar(Lexer lexer) {
  if (lexer->readPosition >= lexer->inputLength) {
    lexer->character = '\0';
  } else {
    lexer->character = lexer->input[lexer->readPosition];
  }

  lexer->position = lexer->readPosition;
  lexer->readPosition++;
}

static void _lexerSkipWhitespace(Lexer lexer) {
  while (lexer->character == ' ' || lexer->character == '\t' ||
         lexer->character == '\n' || lexer->character == '\r') {
    _lexerReadChar(lexer);
  }
}

static const char *_lexerReadIdent(Lexer lexer, size_t *pLen) {
  char *result = NULL;
  size_t position = lexer->position;

  while (_isLetter(lexer->character)) {
    _lexerReadChar(lexer);
  }

  if (pLen) {
    *pLen = lexer->position - position;
  }

  return lexer->input + position;
}

static const char *_lexerReadInt(Lexer lexer, size_t *pLen) {
  char *result = NULL;
  size_t position = lexer->position;

  while (_isNumber(lexer->character)) {
    _lexerReadChar(lexer);
  }

  if (pLen) {
    *pLen = lexer->position - position;
  }

  return lexer->input + position;
}

static uint8_t _isLetter(char ch) {
  return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_';
}

static uint8_t _isNumber(char ch) { return '0' <= ch && '9' >= ch; }

static TokenType _getTokenTypeFromLiteral(const char *literal, size_t len) {
  if (strncmp(literal, "let", len) == 0) {
    return TokenTypeLet;
  } else if (strncmp(literal, "fn", len) == 0) {
    return TokenTypeFunction;
  }

  return TokenTypeIdent;
}

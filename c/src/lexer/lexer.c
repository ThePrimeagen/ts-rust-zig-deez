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
static char *_lexerReadIdent(Lexer lexer);
static char *_lexerReadInt(Lexer lexer);

static uint8_t _isLetter(char ch);
static uint8_t _isNumber(char ch);

static TokenType _getTokenTypeFromLiteral(const char *literal);

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
    /*
     * TODO: Refactor and optimize
     * at the moment memory is always alocated for identifier
     * it's not necessery for keywords
     */
    char *ident = _lexerReadIdent(lexer);
    TokenType type = _getTokenTypeFromLiteral(ident);
    if (type != TokenTypeIdent) {
      free(ident);
      ident = NULL;
    }
    tok = tokenCreate(type, ident);
    return tok;
  } else if (_isNumber(lexer->character)) {
    char *ident = _lexerReadInt(lexer);
    tok = tokenCreate(TokenTypeInt, ident);
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

static char *_lexerReadIdent(Lexer lexer) {
  char *result = NULL;
  size_t position = lexer->position;
  size_t len = 0;

  while (_isLetter(lexer->character)) {
    _lexerReadChar(lexer);
    len++;
  }

  result = strndup(lexer->input + position, len);

  return result;
}

static char *_lexerReadInt(Lexer lexer) {
  char *result = NULL;
  size_t position = lexer->position;
  size_t len = 0;

  while (_isNumber(lexer->character)) {
    _lexerReadChar(lexer);
    len++;
  }

  result = strndup(lexer->input + position, len);

  return result;
}

static uint8_t _isLetter(char ch) {
  return 'a' <= ch && 'z' >= ch || 'A' <= ch && 'Z' >= ch || ch == '_';
}

static uint8_t _isNumber(char ch) { return '0' <= ch && '9' >= ch; }

static TokenType _getTokenTypeFromLiteral(const char *literal) {
  if (strcmp(literal, "let") == 0) {
    return TokenTypeLet;
  } else if (strcmp(literal, "fn") == 0) {
    return TokenTypeFunction;
  }

  return TokenTypeIdent;
}

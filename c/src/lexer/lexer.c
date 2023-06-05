#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "lexer.h"

struct SLexer {
  const char *input;
  size_t inputLength;
  size_t position;
  size_t readPosition;
  char ch;
};

static void _lexerReadChar(Lexer *lexer);
static char _lexerPeek(Lexer *lexer);
static void _lexerSkipWhitespace(Lexer *lexer);
static const char *_lexerReadIdent(Lexer *lexer, size_t *len);
static const char *_lexerReadInt(Lexer *lexer, size_t *len);

static uint8_t _isLetter(char ch);
static uint8_t _isNumber(char ch);

static TokenType _getTokenTypeFromLiteral(const char *literal, size_t len);

/******************************************************************************
                              PUBLIC FUNCTIONS                                *
*******************************************************************************/

Lexer *lexerCreate(const char *input) {
  size_t len = sizeof(Lexer);
  Lexer *lexer = malloc(len);
  memset(lexer, 0, len);

  lexer->input = input;
  lexer->inputLength = strlen(input);
  lexer->position = 0;
  lexer->readPosition = 0;

  _lexerReadChar(lexer);

  return lexer;
}

#include <stdio.h>
Token *lexerNext(Lexer *lexer) {
  Token *tok = NULL;

  _lexerSkipWhitespace(lexer);

  switch (lexer->ch) {
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
  case '-':
    tok = tokenCreate(TokenTypeMinus, NULL);
    break;
  case '=':
    if (_lexerPeek(lexer) == '=') {
      _lexerReadChar(lexer);
      tok = tokenCreate(TokenTypeEqual, NULL);
    } else {
      tok = tokenCreate(TokenTypeAssign, NULL);
    }
    break;
  case '!':
    if (_lexerPeek(lexer) == '=') {
      _lexerReadChar(lexer);
      tok = tokenCreate(TokenTypeNotEqual, NULL);
    } else {
      tok = tokenCreate(TokenTypeBang, NULL);
    }
    break;
  case '/':
    tok = tokenCreate(TokenTypeSlash, NULL);
    break;
  case '*':
    tok = tokenCreate(TokenTypeAsterisk, NULL);
    break;
  case '>':
    tok = tokenCreate(TokenTypeGT, NULL);
    break;
  case '<':
    tok = tokenCreate(TokenTypeLT, NULL);
    break;
  case '\0':
    tok = tokenCreate(TokenTypeEof, NULL);
    break;
  }

  if (_isLetter(lexer->ch)) {
    size_t len = 0;
    char *literal = NULL;
    const char *ident = _lexerReadIdent(lexer, &len);

    TokenType type = _getTokenTypeFromLiteral(ident, len);
    if (type == TokenTypeIdent) {
      literal = strndup(ident, len);
    }

    tok = tokenCreate(type, literal);
    return tok;
  } else if (_isNumber(lexer->ch)) {
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

void lexerCleanup(Lexer **lexer) {
  if (*lexer) {
    free(*lexer);
  }

  *lexer = NULL;
}

Token *tokenCreate(TokenType type, char *literal) {
  size_t len = sizeof(Token);
  Token *token = malloc(len);
  memset(token, 0, len);

  token->literal = literal;
  token->type = type;

  return token;
}

void tokenCleanup(Token **token) {
  if (*token && (*token)->literal) {
    free((*token)->literal);
  }

  if (*token) {
    free(*token);
  }

  *token = NULL;
}

/******************************************************************************
                              PRIVATE FUNCTIONS                               *
*******************************************************************************/

static void _lexerReadChar(Lexer *lexer) {
  if (lexer->readPosition >= lexer->inputLength) {
    lexer->ch = '\0';
  } else {
    lexer->ch = lexer->input[lexer->readPosition];
  }

  lexer->position = lexer->readPosition;
  lexer->readPosition++;
}

static char _lexerPeek(Lexer *lexer) {
  if (lexer->readPosition >= lexer->inputLength) {
    return '\0';
  } else {
    return lexer->input[lexer->readPosition];
  }
}

static void _lexerSkipWhitespace(Lexer *lexer) {
  while (lexer->ch == ' ' || lexer->ch == '\t' || lexer->ch == '\n' ||
         lexer->ch == '\r') {
    _lexerReadChar(lexer);
  }
}

static const char *_lexerReadIdent(Lexer *lexer, size_t *len) {
  char *result = NULL;
  size_t position = lexer->position;

  while (_isLetter(lexer->ch)) {
    _lexerReadChar(lexer);
  }

  if (len) {
    *len = lexer->position - position;
  }

  return lexer->input + position;
}

static const char *_lexerReadInt(Lexer *lexer, size_t *len) {
  char *result = NULL;
  size_t position = lexer->position;

  while (_isNumber(lexer->ch)) {
    _lexerReadChar(lexer);
  }

  if (len) {
    *len = lexer->position - position;
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
  } else if (strncmp(literal, "true", len) == 0) {
    return TokenTypeTrue;
  } else if (strncmp(literal, "false", len) == 0) {
    return TokenTypeFalse;
  } else if (strncmp(literal, "if", len) == 0) {
    return TokenTypeIf;
  } else if (strncmp(literal, "else", len) == 0) {
    return TokenTypeElse;
  } else if (strncmp(literal, "return", len) == 0) {
    return TokenTypeReturn;
  }

  return TokenTypeIdent;
}

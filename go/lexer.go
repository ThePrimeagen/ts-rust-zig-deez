package lexer

import (
	"unicode"
)

type TokenType string

const (
	ILLEGAL   TokenType = "ILLEGAL"
	EOF       TokenType = "EOF"
	IDENT     TokenType = "IDENT"
	INT       TokenType = "INT"
	EQUAL     TokenType = "="
	PLUS      TokenType = "+"
	COMMA     TokenType = ","
	SEMICOLON TokenType = ";"
	LPAREN    TokenType = "("
	RPAREN    TokenType = ")"
	LSQUIRLY  TokenType = "{"
	RSQUIRLY  TokenType = "}"
	FUNCTION  TokenType = "FUNCTION"
	LET       TokenType = "LET"
)

type Token struct {
	Type    TokenType
	Literal string
}

func NewToken(tokenType TokenType, literal string) Token {
	return Token{Type: tokenType, Literal: literal}
}

var Keywords = map[string]TokenType{
	"fn":  FUNCTION,
	"let": LET,
}

type Lexer struct {
	input        string
	position     int
	readPosition int
	ch           rune
}

func NewLexer(input string) *Lexer {
	l := &Lexer{input: input}
	l.readChar()
	return l
}

func (l *Lexer) NextToken() Token {
	l.skipWhitespace()

	if token, ok := getTokenFromMap(l.ch); ok {
		l.readChar()
		return token
	}

	if l.ch == 0 {
		return NewToken(EOF, "")
	}

	if unicode.IsLetter(l.ch) {
		literal := l.readIdentifier()
		if keywordType, ok := Keywords[literal]; ok {
			return NewToken(keywordType, literal)
		}
		return NewToken(IDENT, literal)
	}

	if unicode.IsDigit(l.ch) {
		return NewToken(INT, l.readNumber())
	}

	return NewToken(ILLEGAL, string(l.ch))
}

func getTokenFromMap(ch rune) (Token, bool) {
	var tokenMap = map[rune]Token{
		'=': NewToken(EQUAL, "="),
		'+': NewToken(PLUS, "+"),
		',': NewToken(COMMA, ","),
		';': NewToken(SEMICOLON, ";"),
		'(': NewToken(LPAREN, "("),
		')': NewToken(RPAREN, ")"),
		'{': NewToken(LSQUIRLY, "{"),
		'}': NewToken(RSQUIRLY, "}"),
	}

	token, ok := tokenMap[ch]
	return token, ok
}

func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0
	} else {
		l.ch = rune(l.input[l.readPosition])
	}
	l.position = l.readPosition
	l.readPosition++
}

func (l *Lexer) skipWhitespace() {
	for unicode.IsSpace(l.ch) {
		l.readChar()
	}
}

func (l *Lexer) readIdentifier() string {
	position := l.position
	for unicode.IsLetter(l.ch) {
		l.readChar()
	}
	return l.input[position:l.position]
}

func (l *Lexer) readNumber() string {
	position := l.position
	for unicode.IsDigit(l.ch) {
		l.readChar()
	}
	return l.input[position:l.position]
}

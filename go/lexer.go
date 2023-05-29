package lexer

import (
	"unicode"
)

type TokenType uint8

type Lexer struct {
	input        string
	position     int
	readPosition int
	ch           rune
}

type Token struct {
	Type    TokenType
	Literal string
}

const (
	Illegal TokenType = iota
	Eof
	Ident
	Int
	Equal
	Plus
	Comma
	Semicolon
	LParen
	RParen
	LSquirly
	RSquirly
	Function
	Let
)

var keywords = map[string]TokenType{
	"fn":  Function,
	"let": Let,
}

var tokens = map[rune]Token{
	'=': NewToken(Equal, "="),
	'+': NewToken(Plus, "+"),
	',': NewToken(Comma, ","),
	';': NewToken(Semicolon, ";"),
	'(': NewToken(LParen, "("),
	')': NewToken(RParen, ")"),
	'{': NewToken(LSquirly, "{"),
	'}': NewToken(RSquirly, "}"),
}

func NewLexer(input string) *Lexer {
	l := &Lexer{input: input}
	l.readChar()
	return l
}

func NewToken(tokenType TokenType, literal string) Token {
	return Token{Type: tokenType, Literal: literal}
}

func (l *Lexer) NextToken() Token {
	l.skipWhitespace()

	if token, ok := tokens[l.ch]; ok {
		l.readChar()
		return token
	}

	if l.ch == 0 {
		return NewToken(Eof, "")
	}

	if unicode.IsLetter(l.ch) {
		literal := l.readIdentifier()
		if keywordType, ok := keywords[literal]; ok {
			return NewToken(keywordType, literal)
		}
		return NewToken(Ident, literal)
	}

	if unicode.IsDigit(l.ch) {
		return NewToken(Int, l.readNumber())
	}

	return NewToken(Illegal, string(l.ch))
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

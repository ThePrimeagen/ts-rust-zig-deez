package go_deez

type TokenType string

const (
	Illegal   TokenType = "ILLEGAL"
	Eof       TokenType = "EOF"
	Ident     TokenType = "IDENT"
	Int       TokenType = "INT"
	Equal     TokenType = "="
	Plus      TokenType = "+"
	Comma     TokenType = ","
	Semicolon TokenType = ";"
	LParen    TokenType = "("
	RParen    TokenType = ")"
	LSquirly  TokenType = "{"
	RSquirly  TokenType = "}"
	Function  TokenType = "FUNCTION"
	Let       TokenType = "LET"
)

type Token struct {
	type_   TokenType
	literal string
}

func CreateToken(type_ TokenType, literal string) Token {
	return Token{type_: type_, literal: literal}
}

const _0 = int('0')
const _9 = int('9')

const a = int('a')
const z = int('z')

const A = int('A')
const Z = int('Z')

const __ = int('_')

func isLetter(character rune) bool {
	var char = int(character)
	return a <= char && z >= char ||
		A <= char && Z >= char ||
		char == __
}

func isNumber(character rune) bool {
	var char = int(character)
	return _0 <= char && _9 >= char
}

var Keyword = map[string]Token{
	"fn":  CreateToken(Function, "fn"),
	"let": CreateToken(Let, "let"),
}

type Tokenizer struct {
	position     int
	readPosition int
	ch           rune
	input        string
}

func NewTokenizer(input string) Tokenizer {
	var tokenizer = Tokenizer{
		position:     0,
		readPosition: 0,
		input:        input,
	}
	tokenizer.readChar()
	return tokenizer
}

func (tokenizer *Tokenizer) GetNextToken() Token {
	tokenizer.skipWhitespace()

	var tok Token
	var tokNil bool = true
	switch tokenizer.ch {
	case '{':
		tok = CreateToken(LSquirly, string(tokenizer.ch))
		tokNil = false
	case '}':
		tok = CreateToken(RSquirly, string(tokenizer.ch))
		tokNil = false
	case '(':
		tok = CreateToken(LParen, string(tokenizer.ch))
		tokNil = false
	case ')':
		tok = CreateToken(RParen, string(tokenizer.ch))
		tokNil = false
	case ',':
		tok = CreateToken(Comma, string(tokenizer.ch))
		tokNil = false
	case ';':
		tok = CreateToken(Semicolon, string(tokenizer.ch))
		tokNil = false
	case '+':
		tok = CreateToken(Plus, string(tokenizer.ch))
		tokNil = false
	case '=':
		tok = CreateToken(Equal, string(tokenizer.ch))
		tokNil = false
	case '\x00':
		tok = CreateToken(Eof, "eof")
		tokNil = false
	}

	if isLetter((tokenizer.ch)) {
		var ident = tokenizer.readIdent()
		var keyword, exists = Keyword[ident]
		if exists {
			return keyword
		} else {
			return CreateToken(Ident, ident)
		}
	} else if isNumber((tokenizer.ch)) {
		return CreateToken(Int, tokenizer.readInt())
	} else if tokNil {
		return CreateToken(Illegal, string(tokenizer.ch))
	}

	tokenizer.readChar()
	return tok
}

func (tokenizer *Tokenizer) readChar() {
	if tokenizer.readPosition >= len(tokenizer.input) {
		tokenizer.ch = '\x00'
	} else {
		tokenizer.ch = rune(tokenizer.input[tokenizer.readPosition])
	}

	tokenizer.position = tokenizer.readPosition
	tokenizer.readPosition++
}

func (tokenizer *Tokenizer) skipWhitespace() {
	for tokenizer.ch == ' ' || tokenizer.ch == '\t' || tokenizer.ch == '\n' || tokenizer.ch == '\r' {
		tokenizer.readChar()
	}
}

func (tokenizer *Tokenizer) readIdent() string {
	var position = tokenizer.position

	for isLetter(tokenizer.ch) {
		tokenizer.readChar()
	}

	return tokenizer.input[position:tokenizer.position]
}

func (tokenizer *Tokenizer) readInt() string {
	var position = tokenizer.position

	for isNumber(tokenizer.ch) {
		tokenizer.readChar()
	}

	return tokenizer.input[position:tokenizer.position]
}

package lexer

import (
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"
)

type tokenType int

const (
	tokenIllegal tokenType = iota
	tokenEOF
	tokenIdent
	tokenInt
	tokenAssign
	tokenPlus
	tokenComma
	tokenSemicolon
	tokenLParen
	tokenRParen
	tokenLBrace
	tokenRBrace
	tokenFunction
	tokenLet
)

type token struct {
	typ tokenType
	val string
}

func (t token) String() string {
	switch t.typ {
	case tokenEOF:
		return "EOF"
	case tokenIllegal:
		return t.val
	}
	if len(t.val) >= 10 {
		return fmt.Sprintf("%.10q...", t.val)
	}
	return fmt.Sprintf("%q", t.val)
}

var keywords = map[string]tokenType{
	"fn":  tokenFunction,
	"let": tokenLet,
}

const eof = -1

type Lexer struct {
	input  string
	start  int
	pos    int
	width  int
	tokens chan token
}

func NewLexer(input string) (*Lexer, chan token) {
	l := &Lexer{
		input:  input,
		tokens: make(chan token),
	}
	go l.run() // Concurrently run state machine.
	return l, l.tokens
}

// stateFn represents the state of the scanner
// as a function that returns the next state.
type stateFn func(*Lexer) stateFn

// run lexes the input by executing state functions
// until the state is nil.
func (l *Lexer) run() {
	for state := lex; state != nil; {
		state = state(l)
	}
	close(l.tokens)
}

func lex(l *Lexer) stateFn {
	for {
		switch r := l.next(); {
		case isSpace(r):
			l.ignore()
		case r == '=':
			l.emit(tokenAssign)
		case r == ';':
			l.emit(tokenSemicolon)
		case r == '(':
			l.emit(tokenLParen)
		case r == ')':
			l.emit(tokenRParen)
		case r == ',':
			l.emit(tokenComma)
		case r == '+':
			l.emit(tokenPlus)
		case r == '{':
			l.emit(tokenLBrace)
		case r == '}':
			l.emit(tokenRBrace)
		case '0' <= r && r <= '9':
			l.backup()
			return lexNumber
		case isAlphaNumeric(r):
			l.backup()
			return lexIdent
		case r == eof:
			l.emit(tokenEOF)
			return nil
		default:
			return l.errorf("unrecognized character in action: %#U", r)
		}
	}
}

func lexNumber(l *Lexer) stateFn {
	digits := "0123456789"
	l.acceptRun(digits)

	if isAlphaNumeric(l.peek()) {
		l.next()
		return l.errorf("bad number syntax: %q", l.input[l.start:l.pos])
	}

	l.emit(tokenInt)

	return lex
}

func lexIdent(l *Lexer) stateFn {
	for isAlphaNumeric(l.next()) {
	}
	l.backup()

	if t, ok := keywords[l.input[l.start:l.pos]]; ok {
		l.emit(t)
	} else {
		l.emit(tokenIdent)
	}

	return lex
}

// TODO: still waiting for Tom to implement this
func lexComment(l *Lexer) stateFn {
	return nil
}

func (l *Lexer) next() (r rune) {
	if l.pos >= len(l.input) {
		l.width = 0
		return eof
	}
	r, l.width = utf8.DecodeRuneInString(l.input[l.pos:])
	l.pos += l.width
	return r
}

func (l *Lexer) peek() rune {
	r := l.next()
	l.backup()
	return r
}

func (l *Lexer) backup() {
	l.pos -= l.width
}

func (l *Lexer) emit(t tokenType) {
	l.tokens <- token{t, l.input[l.start:l.pos]}
	l.start = l.pos
}

func (l *Lexer) ignore() {
	l.start = l.pos
}

func (l *Lexer) acceptRun(valid string) {
	for strings.ContainsRune(valid, l.next()) {
	}
	l.backup()
}

func (l *Lexer) errorf(format string, args ...interface{}) stateFn {
	l.tokens <- token{
		tokenIllegal,
		fmt.Sprintf(format, args...),
	}
	return nil
}

func isSpace(r rune) bool {
	return r == ' ' || r == '\t' || r == '\r' || r == '\n'
}

func isAlphaNumeric(r rune) bool {
	return r == '_' || unicode.IsLetter(r) || unicode.IsDigit(r)
}

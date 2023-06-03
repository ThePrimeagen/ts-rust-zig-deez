#define TOKEN_ILLEGAL   ( 0)
#define TOKEN_EOF       ( 1)
#define TOKEN_IDENT     ( 2)
#define TOKEN_INT       ( 3)
#define TOKEN_EQUAL     ( 4)
#define TOKEN_PLUS      ( 5)
#define TOKEN_COMMA     ( 6)
#define TOKEN_SEMICOLON ( 7)
#define TOKEN_L_PAREN   ( 8)
#define TOKEN_R_PAREN   ( 9)
#define TOKEN_L_SQUIRLY (10)
#define TOKEN_R_SQUIRLY (11)
#define TOKEN_FUNCTION  (12)
#define TOKEN_LET       (13)

class Lexer {
	U8 *input;
	U64 input_len;
	U64 position;
	U64 read_position;
	U8 ch;
};

class Token {
	U64 type;
	U8 *literal;
};

U8 *StrNewN(U8 *buf, U64 sz)
{
	U8 *out = MAlloc(sz+1);
	MemCpy(out, buf, sz);
	out[sz] = '\0';
	return out;
}

U0 LexerReadChar(Lexer *l)
{
	if (l->read_position >= l->input_len)
		l->ch = '\0';
	else
		l->ch = l->input[l->read_position];

	l->position = l->read_position;
	l->read_position++;
}

U0 LexerSkipWhitespace(Lexer *l)
{
	while (l->ch == ' ' || l->ch == '\t' || l->ch == '\n')
		LexerReadChar(l);
}

U8 *LexerReadIdent(Lexer *l, U64 *len)
{
	U64 position = l->position;
	while ('a' <= l->ch && l->ch <= 'z' || 'A' <= l->ch && l->ch <= 'Z' || l->ch == '_')
		LexerReadChar(l);
	if (len)
		*len = l->position - position;
	return StrNewN(l->input + position, *len);
}

U8 *LexerReadInt(Lexer *l, U64 *len)
{
	U64 position = l->position;
	while ('0' <= l->ch && l->ch <= '9')
		LexerReadChar(l);
	if (len)
		*len = l->position - position;
	return StrNewN(l->input + position, *len);
}

U64 GetTokenTypeFromLiteral(U8 *ident, U64 len)
{
	U64 type;
	if (StrCmp(ident, "let") == 0)
		type = TOKEN_LET;
	else if (StrCmp(ident, "fn") == 0)
		type = TOKEN_FUNCTION;
	else
		type = TOKEN_IDENT;
	return type;
}

Token *TokenCreate(U64 type, U8 *literal)
{
	Token *tok = MAlloc(sizeof(Token));
	tok->type = type;
	tok->literal = literal;
	return tok;
}

Token *LexerNextToken(Lexer *l)
{
	LexerSkipWhitespace(l);

	Token *tok = NULL;

	U64 len = 0;
	U8 *ident = NULL, *literal = NULL;
	U64 type = 0;
	switch (l->ch) {
		case '{':  tok = TokenCreate(TOKEN_L_SQUIRLY, NULL); break;
		case '}':  tok = TokenCreate(TOKEN_R_SQUIRLY, NULL); break;
		case '(':  tok = TokenCreate(TOKEN_L_PAREN,   NULL); break;
		case ')':  tok = TokenCreate(TOKEN_R_PAREN,   NULL); break;
		case ',':  tok = TokenCreate(TOKEN_COMMA,     NULL); break;
		case ';':  tok = TokenCreate(TOKEN_SEMICOLON, NULL); break;
		case '+':  tok = TokenCreate(TOKEN_PLUS,      NULL); break;
		case '=':  tok = TokenCreate(TOKEN_EQUAL,     NULL); break;
		case '\0': tok = TokenCreate(TOKEN_EOF,       NULL); break;
		case 'A'...'Z':
		case 'a'...'z':
		case '_':
			ident = LexerReadIdent(l, &len);
			type = GetTokenTypeFromLiteral(ident, len);
			if (type == TOKEN_IDENT)
				literal = ident;

			tok = TokenCreate(type, literal);
			return tok;
		case '0'...'9':
			ident = LexerReadInt(l, &len);
			tok = TokenCreate(TOKEN_INT, ident);
			return tok;
		default:
			return TokenCreate(TOKEN_ILLEGAL, NULL);
	}

	if (!tok)
		tok = TokenCreate(TOKEN_ILLEGAL, NULL);

	LexerReadChar(l);

	return tok;
}

Lexer *LexerInit(U8 *input)
{
	Lexer *l = MAlloc(sizeof(Lexer));
	l->input = input;
	l->input_len = StrLen(input);
	l->position = 0;
	l->read_position = 0;
	l->ch = 0;

	LexerReadChar(l);

	return l;
}

U0 TokenFree(Token **tok) {
	if (*tok && (*tok)->literal)
		Free((*tok)->literal);
	Free(*tok);
	*tok = NULL;
}


// "Entertain God and your ass will follow." - Terry A. Davis

class Token {
	U64 type;
	U8 *literal;
};

U8 *TOKEN_STR = "ILLEGAL\0"
	"EOF\0"
	"ASSIGN\0"
	"IDENT\0"
	"INT\0"
	"PLUS\0"
	"COMMA\0"
	"SEMICOLON\0"
	"L_PAREN\0"
	"R_PAREN\0"
	"L_SQUIRLY\0"
	"R_SQUIRLY\0"
	"FUNCTION\0"
	"BANG\0"
	"DASH\0"
	"FORWARD_SLASH\0"
	"ASTERISK\0"
	"LESS_THAN\0"
	"GREATER_THAN\0"
	"EQUAL\0"
	"NOT_EQUAL\0"
	"LET\0"
	"IF\0"
	"ELSE\0"
	"RETURN\0"
	"FALSE\0"
	"TRUE\0";

#exe {
	I64 offset;
	I64 i = 0;
	for (offset=0;TOKEN_STR[offset+1];offset+=StrLen(TOKEN_STR+offset)+1)
		StreamPrint("#define TOKEN_%s (%d)\n", TOKEN_STR+offset, i++);
};

U8 *TokenPretty(Token *tok)
{
	switch (tok->type) {
#exe {
	I64 offset;
	for (offset=0;TOKEN_STR[offset+1];offset+=StrLen(TOKEN_STR+offset)+1)
		StreamPrint("\tcase TOKEN_%s: return \"%s\";\n", TOKEN_STR+offset, TOKEN_STR+offset);
}
	}
	return "ERROR";
}


class Lexer {
	U8 *input;
	U64 input_len;
	U64 position;
	U64 read_position;
	U8 ch;
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

U8 LexerPeekChar(Lexer *l)
{
	if (l->read_position >= l->input_len)
		l->ch = '\0';
	else
		l->ch = l->input[l->read_position];

  return l->ch;
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

U64 GetTokenTypeFromLiteral(U8 *ident)
{
	U64 type;
	if (StrCmp(ident, "let") == 0)
		type = TOKEN_LET;
	else if (StrCmp(ident, "fn") == 0)
		type = TOKEN_FUNCTION;
	else if (StrCmp(ident, "if") == 0)
    type = TOKEN_IF;
	else if (StrCmp(ident, "else") == 0)
    type = TOKEN_ELSE;
	else if (StrCmp(ident, "return") == 0)
    type = TOKEN_RETURN;
	else if (StrCmp(ident, "false") == 0)
    type = TOKEN_FALSE;
	else if (StrCmp(ident, "true") == 0)
    type = TOKEN_TRUE;
	else
		type = TOKEN_IDENT;
	return type;
}

Token *TokenCreate(U64 type, U8 *literal=NULL)
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
		case '{':  tok = TokenCreate(TOKEN_L_SQUIRLY    ); break;
		case '}':  tok = TokenCreate(TOKEN_R_SQUIRLY    ); break;
		case '(':  tok = TokenCreate(TOKEN_L_PAREN      ); break;
		case ')':  tok = TokenCreate(TOKEN_R_PAREN      ); break;
		case ',':  tok = TokenCreate(TOKEN_COMMA        ); break;
		case ';':  tok = TokenCreate(TOKEN_SEMICOLON    ); break;
		case '+':  tok = TokenCreate(TOKEN_PLUS         ); break;
		case '-':  tok = TokenCreate(TOKEN_DASH         ); break;
		case '/':  tok = TokenCreate(TOKEN_FORWARD_SLASH); break;
		case '*':  tok = TokenCreate(TOKEN_ASTERISK     ); break;
		case '<':  tok = TokenCreate(TOKEN_LESS_THAN    ); break;
		case '>':  tok = TokenCreate(TOKEN_GREATER_THAN ); break;
		case '\0': tok = TokenCreate(TOKEN_EOF          ); break;
		case '!':
      if (LexerPeekChar(l) == '=') {
        LexerReadChar(l);
		    tok = TokenCreate(TOKEN_NOT_EQUAL);
      } else
		    tok = TokenCreate(TOKEN_BANG);
      break;
		case '=':
      if (LexerPeekChar(l) == '=') {
        LexerReadChar(l);
		    tok = TokenCreate(TOKEN_EQUAL);
      } else
        tok = TokenCreate(TOKEN_ASSIGN);
      break;
		case 'A'...'Z':
		case 'a'...'z':
		case '_':
			ident = LexerReadIdent(l, &len);
			type = GetTokenTypeFromLiteral(ident);
			if (type == TOKEN_IDENT)
				literal = ident;

			tok = TokenCreate(type, literal);
			return tok;
		case '0'...'9':
			ident = LexerReadInt(l, &len);
			tok = TokenCreate(TOKEN_INT, ident);
			return tok;
		default:
			return TokenCreate(TOKEN_ILLEGAL);
	}

	if (!tok)
		tok = TokenCreate(TOKEN_ILLEGAL);

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


#include "Lexer.HC"

// Technically, this shouldn't need to be put here, but for some reason the compiler errors out when put inside the while loop. IDK. May be a 3Days issue.
Lexer *lex;
Token *tok;
U8 *str;
while (TRUE) {
	str = GetStr("> ");
	if (StrCmp(str, "quit") == 0)
		break;

	lex = LexerInit(str);
	do {
		tok = LexerNextToken(lex);
		"%s", TokenPretty(tok);
		if (tok->type == TOKEN_IDENT || tok->type == TOKEN_INT)
			" (%s)", tok->literal;
		"\n";
	} while (tok->type != TOKEN_EOF);

	Free(lex);
	Free(str);
}

Free(str);


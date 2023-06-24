#include "Parser.HC"

// Technically, this shouldn't need to be put here, but for some reason the compiler errors out when put inside the while loop. IDK. May be a 3Days issue.
Lexer *lex;
Parser *parser;
Node *root;
U8 *str;
while (TRUE) {
	str = GetStr("> ");
	if (StrCmp(str, "quit") == 0)
		break;

	lex = LexerNew(str);
	parser = ParserNew(lex);
	
	root = ParseProg(parser);
	NodePretty(root);

	Free(parser);
	Free(lex);
}

Free(str);


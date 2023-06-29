#include "Parser.HC"

// Technically, this shouldn't need to be put here, but for some reason the compiler errors out when put inside the while loop. IDK. May be a 3Days issue.
Lexer *lex;
Parser *parser;
Node *root;
U8 *str;
U64 i;
U64 mem_loc;
U64 *tmp;
while (TRUE) {
	str = GetStr("> ");
	if (StrCmp(str, "quit") == 0)
		break;

	lex = LexerNew(str);
	parser = ParserNew(lex);

	root = ParseProg(parser);

	NodePretty(root);
	if (parser->errors->len > 0) {
		"Errors found:\n";

		for (i = 0; i < parser->errors->len; i++) {
			tmp = &parser->errors->d[parser->errors->size * i];
			mem_loc = *tmp;
			// This is a huge hack cause I don't want to figure out where I went wrong. God laughs.
			if (mem_loc < 0xffffffffffff)
				" %d - %s \n", i + 1, mem_loc;
		}
	}

	Free(parser);
	Free(lex);
}

Free(str);


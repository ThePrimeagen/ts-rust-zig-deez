#include "Interpreter.HC"

U0 REPL() {
	Lexer *lex;
	Parser *parser;
	Node *root;
	Object *ret;
	Environment env;
	U8 *str;
	U64 i;
	U64 mem_loc;
	U64 *tmp;

	EnvironmentNew(&env);

	while (TRUE) {
		str = GetStr("> ");
		if (StrCmp(str, "quit") == 0)
			break;

		if (StrLen(str) == 0)
			goto next_repl_iter;

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

		if (root && parser->errors->len == 0) {
			ret = Eval(root, &env);
			U8 *lefunny = MObjectPrint(ret);
			"Ret: %s\n", lefunny;
			Free(lefunny);
		};

		Free(parser);
		Free(lex);
next_repl_iter:
		Free(str);
	}
}

REPL;


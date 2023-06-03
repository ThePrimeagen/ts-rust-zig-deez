#include "Lexer.HC"

U0 Error(U8 *fmt, ...) {
	U8 *buf	= StrPrintJoin(NULL, fmt, argc, argv);
	" [FAIL]\n$$RED$$$$BK,1$$Error$$FG$$$$BK,0$$: %s\n", buf;
	Free(buf);
	throw('ASS');
}

"Running tests...\n";
"Test: One";

U64 tests[9] = {
	TOKEN_EQUAL,
	TOKEN_PLUS,
	TOKEN_L_PAREN,
	TOKEN_R_PAREN,
	TOKEN_L_SQUIRLY,
	TOKEN_R_SQUIRLY,
	TOKEN_COMMA,
	TOKEN_SEMICOLON,
	TOKEN_EOF,
};

Lexer *lexer;
lexer = LexerInit("=+(){},;");

I64 i;
for (i = 0; i < 9; i++) {
	Token *token = NULL;
	token = LexerNextToken(lexer);
	if (token->type != tests[i]) {
		Error("Invaild type (%d): %d != %d", i, token->type, tests[i]);
	}
	TokenFree(&token);
}

Free(lexer);

" [OK]\nTest: Complete";

Token *tests2[37] = {
	TokenCreate(TOKEN_LET,       NULL            ),
	TokenCreate(TOKEN_IDENT,     StrNew("five")  ),
	TokenCreate(TOKEN_EQUAL,     NULL            ),
	TokenCreate(TOKEN_INT,       StrNew("5")     ),
	TokenCreate(TOKEN_SEMICOLON, NULL            ),
	TokenCreate(TOKEN_LET,       NULL            ),
	TokenCreate(TOKEN_IDENT,     StrNew("ten")   ),
	TokenCreate(TOKEN_EQUAL,     NULL            ),
	TokenCreate(TOKEN_INT,       StrNew("10")    ),
	TokenCreate(TOKEN_SEMICOLON, NULL            ),
	TokenCreate(TOKEN_LET,       NULL            ),
	TokenCreate(TOKEN_IDENT,     StrNew("add")   ),
	TokenCreate(TOKEN_EQUAL,     NULL            ),
	TokenCreate(TOKEN_FUNCTION,  NULL            ),
	TokenCreate(TOKEN_L_PAREN,   NULL            ),
	TokenCreate(TOKEN_IDENT,     StrNew("x")     ),
	TokenCreate(TOKEN_COMMA,     NULL            ),
	TokenCreate(TOKEN_IDENT,     StrNew("y")     ),
	TokenCreate(TOKEN_R_PAREN,   NULL            ),
	TokenCreate(TOKEN_L_SQUIRLY, NULL            ),
	TokenCreate(TOKEN_IDENT,     StrNew("x")     ),
	TokenCreate(TOKEN_PLUS,      NULL            ),
	TokenCreate(TOKEN_IDENT,     StrNew("y")     ),
	TokenCreate(TOKEN_SEMICOLON, NULL            ),
	TokenCreate(TOKEN_R_SQUIRLY, NULL            ),
	TokenCreate(TOKEN_SEMICOLON, NULL            ),
	TokenCreate(TOKEN_LET,       NULL            ),
	TokenCreate(TOKEN_IDENT,     StrNew("result")),
	TokenCreate(TOKEN_EQUAL,     NULL            ),
	TokenCreate(TOKEN_IDENT,     StrNew("add")   ),
	TokenCreate(TOKEN_L_PAREN,   NULL            ),
	TokenCreate(TOKEN_IDENT,     StrNew("five")  ),
	TokenCreate(TOKEN_COMMA,     NULL            ),
	TokenCreate(TOKEN_IDENT,     StrNew("ten")   ),
	TokenCreate(TOKEN_R_PAREN,   NULL            ),
	TokenCreate(TOKEN_SEMICOLON, NULL            ),
	TokenCreate(TOKEN_EOF,       NULL            ),
};

Lexer *l = LexerInit("let five = 5;"
                     "let ten = 10;"
                     "let add = fn(x, y) { x + y; };"
                     "let result = add(five, ten);");

for (i = 0; i < 37; i++) {
	Token *t = NULL;
	t = LexerNextToken(l);
	if (t->literal && StrCmp(t->literal, tests2[i]->literal) != 0)
		Error("Invalid literals (%d): `%s` != `%s`", i, t->literal, tests2[i]->literal);
	if (t->type != tests2[i]->type)
		Error("Invaild type (%d): %d != %d", i, t->type, tests2[i]->type);
	TokenFree(&t);
	TokenFree(&tests2[i]);
}

Free(l);

" [OK]\nTests done.\n";


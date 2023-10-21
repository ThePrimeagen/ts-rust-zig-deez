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
	TOKEN_ASSIGN,
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

" [OK]\nTest: Old Full";

Token *tests2[37] = {
	TokenCreate(TOKEN_LET),
	TokenCreate(TOKEN_IDENT,     StrNew("five")),
	TokenCreate(TOKEN_ASSIGN),
	TokenCreate(TOKEN_INT,       StrNew("5")),
	TokenCreate(TOKEN_SEMICOLON),
	TokenCreate(TOKEN_LET),
	TokenCreate(TOKEN_IDENT,     StrNew("ten")),
	TokenCreate(TOKEN_ASSIGN),
	TokenCreate(TOKEN_INT,       StrNew("10")),
	TokenCreate(TOKEN_SEMICOLON),
	TokenCreate(TOKEN_LET),
	TokenCreate(TOKEN_IDENT,     StrNew("add")),
	TokenCreate(TOKEN_ASSIGN),
	TokenCreate(TOKEN_FUNCTION),
	TokenCreate(TOKEN_L_PAREN),
	TokenCreate(TOKEN_IDENT,     StrNew("x")),
	TokenCreate(TOKEN_COMMA),
	TokenCreate(TOKEN_IDENT,     StrNew("y")),
	TokenCreate(TOKEN_R_PAREN),
	TokenCreate(TOKEN_L_SQUIRLY),
	TokenCreate(TOKEN_IDENT,     StrNew("x")),
	TokenCreate(TOKEN_PLUS),
	TokenCreate(TOKEN_IDENT,     StrNew("y")),
	TokenCreate(TOKEN_SEMICOLON),
	TokenCreate(TOKEN_R_SQUIRLY),
	TokenCreate(TOKEN_SEMICOLON),
	TokenCreate(TOKEN_LET),
	TokenCreate(TOKEN_IDENT,     StrNew("result")),
	TokenCreate(TOKEN_ASSIGN),
	TokenCreate(TOKEN_IDENT,     StrNew("add")),
	TokenCreate(TOKEN_L_PAREN),
	TokenCreate(TOKEN_IDENT,     StrNew("five")),
	TokenCreate(TOKEN_COMMA),
	TokenCreate(TOKEN_IDENT,     StrNew("ten")),
	TokenCreate(TOKEN_R_PAREN),
	TokenCreate(TOKEN_SEMICOLON),
	TokenCreate(TOKEN_EOF),
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

" [OK]\nTest: Full";

Token *tests3[74] = {
	TokenCreate(TOKEN_LET),
	TokenCreate(TOKEN_IDENT,     StrNew("five")),
	TokenCreate(TOKEN_ASSIGN,    NULL            ),
	TokenCreate(TOKEN_INT,       StrNew("5")),
	TokenCreate(TOKEN_SEMICOLON),
	TokenCreate(TOKEN_LET),
	TokenCreate(TOKEN_IDENT,     StrNew("ten")),
	TokenCreate(TOKEN_ASSIGN,    NULL            ),
	TokenCreate(TOKEN_INT,       StrNew("10")),
	TokenCreate(TOKEN_SEMICOLON),
	TokenCreate(TOKEN_LET),
	TokenCreate(TOKEN_IDENT,     StrNew("add")),
	TokenCreate(TOKEN_ASSIGN,    NULL            ),
	TokenCreate(TOKEN_FUNCTION),
	TokenCreate(TOKEN_L_PAREN),
	TokenCreate(TOKEN_IDENT,     StrNew("x")),
	TokenCreate(TOKEN_COMMA),
	TokenCreate(TOKEN_IDENT,     StrNew("y")),
	TokenCreate(TOKEN_R_PAREN),
	TokenCreate(TOKEN_L_SQUIRLY),
	TokenCreate(TOKEN_IDENT,     StrNew("x")),
	TokenCreate(TOKEN_PLUS),
	TokenCreate(TOKEN_IDENT,     StrNew("y")),
	TokenCreate(TOKEN_SEMICOLON),
	TokenCreate(TOKEN_R_SQUIRLY),
	TokenCreate(TOKEN_SEMICOLON),
	TokenCreate(TOKEN_LET),
	TokenCreate(TOKEN_IDENT,     StrNew("result")),
	TokenCreate(TOKEN_ASSIGN,    NULL            ),
	TokenCreate(TOKEN_IDENT,     StrNew("add")),
	TokenCreate(TOKEN_L_PAREN),
	TokenCreate(TOKEN_IDENT,     StrNew("five")),
	TokenCreate(TOKEN_COMMA),
	TokenCreate(TOKEN_IDENT,     StrNew("ten")),
	TokenCreate(TOKEN_R_PAREN),
	TokenCreate(TOKEN_SEMICOLON),
	TokenCreate(TOKEN_BANG),
  TokenCreate(TOKEN_DASH),
  TokenCreate(TOKEN_FORWARD_SLASH),
  TokenCreate(TOKEN_ASTERISK),
	TokenCreate(TOKEN_INT,       StrNew("5")),
	TokenCreate(TOKEN_SEMICOLON),
	TokenCreate(TOKEN_INT,       StrNew("5")),
	TokenCreate(TOKEN_LESS_THAN),
	TokenCreate(TOKEN_INT,       StrNew("10")),
	TokenCreate(TOKEN_GREATER_THAN),
	TokenCreate(TOKEN_INT,       StrNew("5")),
	TokenCreate(TOKEN_SEMICOLON),
	TokenCreate(TOKEN_IF),
	TokenCreate(TOKEN_L_PAREN),
	TokenCreate(TOKEN_INT,       StrNew("5")),
	TokenCreate(TOKEN_LESS_THAN),
	TokenCreate(TOKEN_INT,       StrNew("10")),
	TokenCreate(TOKEN_R_PAREN),
	TokenCreate(TOKEN_L_SQUIRLY),
	TokenCreate(TOKEN_RETURN),
	TokenCreate(TOKEN_TRUE),
	TokenCreate(TOKEN_SEMICOLON),
	TokenCreate(TOKEN_R_SQUIRLY),
	TokenCreate(TOKEN_ELSE),
	TokenCreate(TOKEN_L_SQUIRLY),
	TokenCreate(TOKEN_RETURN),
	TokenCreate(TOKEN_FALSE),
	TokenCreate(TOKEN_SEMICOLON),
	TokenCreate(TOKEN_R_SQUIRLY),
	TokenCreate(TOKEN_INT,       StrNew("10")),
	TokenCreate(TOKEN_EQUAL),
	TokenCreate(TOKEN_INT,       StrNew("10")),
	TokenCreate(TOKEN_SEMICOLON),
	TokenCreate(TOKEN_INT,       StrNew("10")),
	TokenCreate(TOKEN_NOT_EQUAL),
	TokenCreate(TOKEN_INT,       StrNew("9")),
	TokenCreate(TOKEN_SEMICOLON),
	TokenCreate(TOKEN_EOF),
};

l = LexerInit("let five = 5;"
              "let ten = 10;"
              "let add = fn(x, y) { x + y; };"
              "let result = add(five, ten);"
              "!-/*5;"
              "5 < 10 > 5;"
              "if (5 < 10) {"
              "    return true;"
              "} else {"
              "    return false;"
              "}"
              "10 == 10;"
              "10 != 9;");

for (i = 0; i < 74; i++) {
	Token *t = NULL;
	t = LexerNextToken(l);
	if (t->literal && StrCmp(t->literal, tests3[i]->literal) != 0)
		Error("Invalid literals (%d): `%s` != `%s`", i, t->literal, tests3[i]->literal);
	if (t->type != tests3[i]->type)
		Error("Invaild type (%d): %d != %d", i, t->type, tests3[i]->type);
	TokenFree(&t);
	TokenFree(&tests3[i]);
}


" [OK]\nTests done.\n";
Free(l);


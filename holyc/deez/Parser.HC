#include "AST.HC"

class Parser
{
	Lexer *l;
	Token *c, *n;

	Vector *errors;
};

Token *ParserNext(Parser *p)
{
	p->c = p->n;
	p->n = LexerNextToken(p->l);
	return p->c;
}

Parser *ParserNew(Lexer *l)
{
	Parser *p = MAlloc(sizeof(Parser));
	p->l = l;
	p->errors = NULL;
	VectorNew(&p->errors, sizeof(U8 *), 8);
	// Set c and n
	ParserNext(p);
	ParserNext(p);
	return p;
}

Token *ParserPrevNext(Parser *p)
{
	Token *old = p->c;
	ParserNext(p);
	return old;
}

Bool ParserAccept(Parser *p, U64 type)
{
	if (p->c->type == type) {
		ParserNext(p);
		return TRUE;
	}
	return FALSE;
}

Bool ParserExpect(Parser *p, U64 type)
{
	if (ParserAccept(p, type))
		return TRUE;
	U8 *a = MStrPrint("Unexpected token %s, expected %s", TokenPretty(p->c->type), TokenPretty(type));
	VectorAdd(p->errors, &a);
	return FALSE;
}

// Some macros so type access is easier lol, please don't hate me for this
#define PCT p->c->type
#define PNT p->c->type

Node *(*parse_expr_)(Parser *p); // Cannot use prototyes, use a function pointer instead

Node *ParsePrimary(Parser *p)
{
	Node *node = NULL;

	if (PCT == TOKEN_INT) {
		node = NodeFromToken(p->c);
		ParserNext(p);
	} else if (PCT == TOKEN_IDENT) {
		node = NodeFromToken(p->c);
		ParserNext(p);
	} else if (ParserAccept(p, TOKEN_L_PAREN)) {
		node = parse_expr_(p);
		ParserExpect(p, TOKEN_R_PAREN);
	} else {
		VectorAdd(p->errors, "Unexpected token");
	}

	return node;
}

Node *ParseTerm(Parser *p)
{
	Node *node = ParsePrimary(p);
	while (PCT == TOKEN_ASTERISK || PCT == TOKEN_FORWARD_SLASH) {
		Node *new = NodeFromToken(p->c);
		ParserNext(p);
		VectorAdd(new->children, node);
		VectorAdd(new->children, ParsePrimary(p));
		node = new;
	}
	return node;
}

Node *ParseMathExpr(Parser *p)
{
	Node *node = ParseTerm(p);
	while (PCT == TOKEN_PLUS || PCT == TOKEN_DASH) {
		Node *new = NodeFromToken(p->c);
		ParserNext(p);
		VectorAdd(new->children, node);
		VectorAdd(new->children, ParseTerm(p));
		node = new;
	}

	return node;
}

Node *ParseFunction(Parser *p)
{
	// FIXME: Implement.
	return NULL;
}

Node *ParseExpr(Parser *p)
{
	if (p->c->type == TOKEN_FUNCTION)
		return ParseFunction(p);
	return ParseMathExpr(p);
}
parse_expr_ = &ParseExpr; // Set the function pointer defined at the start

Node *ParseReturn(Parser *p)
{
	Node *node = NodeNew(TOKEN_RETURN);
	Token *data = ParseExpr(p);
	VectorAdd(node->children, data);
	return node;
}

Node *ParseLet(Parser *p)
{
	U8 *a;
	Token *prev = ParserPrevNext(p);
	if (prev->type != TOKEN_IDENT) {
		a = MStrPrint("Unexpected token %s, expected %s", TokenPretty(prev->type), TokenPretty(TOKEN_IDENT));
		VectorAdd(p->errors, &a);
		for (; p->c->type != TOKEN_SEMICOLON && p->c->type != TOKEN_EOF; ParserNext(p));
		return NULL;
	}
	Node *node = NodeNew(TOKEN_LET);
	Node *name = NodeFromToken(prev);
	VectorAdd(node->children, name);
	ParserExpect(p, TOKEN_ASSIGN);
	Node *data = ParseExpr(p);
	VectorAdd(node->children, data);
	return node;
}

Node *ParseProg(Parser *p)
{
	Node *prog = NodeNew(NODE_PROG);
	Node *stmt = NULL;
	U8 *a = NULL;
	for (; PCT != TOKEN_EOF; ParserNext(p)) {
		if (ParserAccept(p, TOKEN_SEMICOLON))
			goto next_token;

		if (ParserAccept(p, TOKEN_RETURN))
			stmt = ParseReturn(p);
		else if (ParserAccept(p, TOKEN_LET))
			stmt = ParseLet(p);
		else
			stmt = ParseExpr(p);

		if (stmt) {
			VectorAdd(prog->children, stmt);
			if (p->c->type != TOKEN_SEMICOLON) {
				a = MStrPrint("Expected %s", TokenPretty(TOKEN_SEMICOLON));
				VectorAdd(p->errors, &a);
			}
		}
next_token:
	}
	return prog;
}


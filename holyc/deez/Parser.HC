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
	// Set c and n
	ParserNext(p);
	ParserNext(p);
	p->errors = VectorNew(sizeof(U8 *), 8);
	return p;
}

Token *ParserPrevNext(Parser *p)
{
	Token *old = p->c;
	ParserNext(p);
	return old;
}

Parser *ParserAccept(Parser *p, U64 type)
{
	if (p->c->type == type) {
		ParserNext(p);
		return TRUE;
	}
	return FALSE;
}

Parser *ParserExpect(Parser *p, U64 type)
{
	if (ParserAccept(p, type))
		return TRUE;
	VectorAdd(p->errors, "Unexpected token");
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

Node *ParseExpr(Parser *p)
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
parse_expr_ = &ParseExpr; // Set the function pointer defined at the start

Node *ParseProg(Parser *p)
{
	Node *prog = NodeNew(NODE_PROG);
	for (; PCT != TOKEN_EOF; ParserNext(p)) {
		Node *stmt;
		//if (PCT == TOKEN_RETURN)
		//	stmt = ParseReturn(p);
		//else if (PCT == TOKEN_LET)
		//	stmt = ParseLet(p);
		//else
			stmt = ParseExpr(p);

		if (stmt)
			VectorAdd(prog->children, stmt);
	}
	return prog;
}


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

// Some macros so type access is easier lol, please don't hate me for this, as a human, I am lazy :^)
#define PCT p->c->type
#define PNT p->n->type

Node *(*parse_expr_)(Parser *p); // Cannot use prototyes, use a function pointer instead
Node *(*parse_prog_)(Parser *p);

Node *ParseFunctionCallArguments(Parser *p)
{
	Node *node = NodeNew(NODE_FUNCTION_ARG_LIST), *ident;
	if (PCT == TOKEN_R_PAREN)
		return node;

	ident = parse_expr_(p);
	VectorAdd(node->children, ident);
	while (PCT == TOKEN_COMMA) {
		ParserNext(p);
		ident = parse_expr_(p);
		VectorAdd(node->children, ident);
	}

	return node;
}

Node *ParseFunctionCall(Parser *p)
{
	Node *name = NodeFromToken(p->c, OFF);
	ParserNext(p);
	if (!ParserExpect(p, TOKEN_L_PAREN))
		return NULL;
	Node *args = ParseFunctionCallArguments(p);
	ParserExpect(p, TOKEN_R_PAREN);
	Node *call = NodeNew(NODE_FUNCTION_CALL);
	VectorAdd(call->children, name);
	VectorAdd(call->children, args);
	return call;
}

Node *ParseIf(Parser *p)
{
	U8 *a;
	ParserExpect(p, TOKEN_L_PAREN);
	Node *expr = parse_expr_(p);
	if (!expr) {
		a = MStrPrint("Expected condition expression");
		VectorAdd(p->errors, &a);
		return NULL;
	}
	ParserExpect(p, TOKEN_R_PAREN);
	ParserExpect(p, TOKEN_L_SQUIRLY);

	Node *if_node = NodeNew(TOKEN_IF);
	VectorAdd(if_node->children, expr);

	Node *stmt_list = parse_prog_(p);
	if (stmt_list)
		VectorAdd(if_node->children, stmt_list);

	ParserExpect(p, TOKEN_R_SQUIRLY);

	if (PCT != TOKEN_ELSE)
		return if_node;

	ParserExpect(p, TOKEN_ELSE);

	ParserExpect(p, TOKEN_L_SQUIRLY);
	stmt_list = parse_prog_(p);
	if (stmt_list)
		VectorAdd(if_node->children, stmt_list);
	ParserExpect(p, TOKEN_R_SQUIRLY);

	return if_node;
}

Node *ParsePrimary(Parser *p)
{
	Node *node = NULL;

	if (PCT == TOKEN_INT) {
		node = NodeFromToken(p->c);
		ParserNext(p);
	} else if (PCT == TOKEN_IDENT) {
		if (PNT == TOKEN_L_PAREN)
			return ParseFunctionCall(p);
		node = NodeFromToken(p->c);
		ParserNext(p);
	} else if (ParserAccept(p, TOKEN_L_PAREN)) {
		node = parse_expr_(p);
		ParserExpect(p, TOKEN_R_PAREN);
	} else if (ParserAccept(p, TOKEN_IF)) {
		return ParseIf(p);
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

Node *ParseFunctionParams(Parser *p)
{
	Node *node = NodeNew(NODE_FUNCTION_DEF_ARG_LIST), *ident;
	if (PCT == TOKEN_R_PAREN)
		return node;

	ident = NodeFromToken(p->c);
	VectorAdd(node->children, ident);
	ParserNext(p);
	while (PCT == TOKEN_COMMA) {
		ParserNext(p);
		ident = NodeFromToken(p->c);
		VectorAdd(node->children, ident);
		ParserNext(p);
	}

	return node;
}

Node *ParseFunction(Parser *p)
{
	ParserExpect(p, TOKEN_L_PAREN);
	Node *params = ParseFunctionParams(p);
	ParserExpect(p, TOKEN_R_PAREN);
	ParserExpect(p, TOKEN_L_SQUIRLY);
	Node *prog = parse_prog_(p);
	ParserExpect(p, TOKEN_R_SQUIRLY);
	Node *node = NodeNew(TOKEN_FUNCTION);
	VectorAdd(node->children, params);
	VectorAdd(node->children, prog);
	return node;
}

Node *ParseExpr(Parser *p)
{
	if (ParserAccept(p, TOKEN_FUNCTION))
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
		if (PCT == TOKEN_R_SQUIRLY)
			break;
	}
	return prog;
}

parse_prog_ = &ParseProg;


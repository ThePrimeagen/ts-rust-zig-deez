#include "Lexer.HC"
#include "Vector.HC"

// Get the type from the token for basic stuff
#define NODE_PROG (255)

U8 *NodeName(U64 type)
{
	if (type == NODE_PROG)
		return "PROGRAM";
	return TokenPretty(type);
}

union NodeValue
{
	I64 i;
	U8 *s;
};

class Node
{
	U64 type;
	NodeValue v;

	Vector *children;
};

U0 NodePretty(Node *n, U8 *indent="", Bool last=TRUE)
{
	"%s", indent;
	if (last) {
		'\\-';
		indent = MStrPrint("%s  ", indent);
	} else {
		'|-';
		indent = MStrPrint("%s| ", indent);
	}

	" %s    ", NodeName(n->type);
	if (n->type == TOKEN_INT)
		"%d", n->v.i;
	if (n->type == TOKEN_IDENT)
		"%s", n->v.s;
	'\n';

	U64 i;
	for (i = 0; i < n->children->len; i++)
		NodePretty(n->children->d + i*n->children->size, indent, i == n->children->len - 1);
}

Node *NodeNew(U64 type)
{
	Node *node = MAlloc(sizeof(Node));
	node->type = type;
	node->v.i = 0;
	node->children = VectorNew(sizeof(Node), 8); // Most tokens will have at most 8 children. How do I know this? idk i just like the number 8 lol.
	return node;
}

Node *NodeFromToken(Token *tok, Bool free_tok=OFF)
{
	NodeValue lol;
	lol.i = 0;
	if (tok->type == TOKEN_INT)
		lol.i = Str2I64(tok->literal);
	if (tok->type == TOKEN_IDENT)
		lol.i = StrNew(tok->literal);
	if (free_tok)
		Free(tok);
	Node *final = NodeNew(tok->type);
	MemCpy(&final->v, &lol, sizeof(NodeValue));
	return final;
}


#include "Parser.HC"

#define OBJ_NIL     (0)
#define OBJ_INT     (1)
#define OBJ_BOOLEAN (2)

class Object
{
	U64 type;
	NodeValue v;
};

Object *ObjectNewI64(I64 v)
{
	Object *obj = MAlloc(sizeof(Object));
	obj->type = OBJ_INT;
	obj->v.i = v;
	return obj;
}

Object *ObjectNewBool(Bool v)
{
	Object *obj = MAlloc(sizeof(Object));
	obj->type = OBJ_BOOLEAN;
	obj->v.i = v;
	return obj;
}

Object *ObjectNewNil()
{
	Object *obj = MAlloc(sizeof(Object));
	obj->type = OBJ_NIL;
	obj->v.i = 0;
	return obj;
}

U8 *MObjectPrint(Object *obj)
{
	if (obj == NULL)
		return StrNew("None");
	if (obj->type == OBJ_NIL)
		return MStrPrint("null", obj->v.i);
	if (obj->type == OBJ_INT)
		return MStrPrint("%d (Integer)", obj->v.i);
	if (obj->type == OBJ_BOOLEAN)
		return MStrPrint("%d (Boolean)", obj->v.i);
	return StrNew("Unknown");
}

class Symbol : Object
{
	U8 *name;
};

class Environment
{
	Vector *symbols; // Symbol
	Environment *parent;
};

U0 EnvironmentNew(Environment *env)
{
	VectorNew(&env->symbols, sizeof(Symbol));
	env->parent = NULL;
}

Object *EnvironmentLookupAdd(Environment *env, U8 *symbol_name, Object *creation_object=NULL)
{
	Symbol *current = NULL;
	Environment *current_env = env;
	I64 i;

	for (; current_env != NULL; current_env = current_env->parent) {
		for (i=0; i<current_env->symbols->len; i++) {
			current = &current_env->symbols->d[current_env->symbols->size * i];
			//"Current var: %s, target: %s\n", current->name, symbol_name;
			if (StrCmp(current->name, symbol_name) == 0) // FIXME: If creation_object is not NULL, send error.
				return current;
		}
	}

	if (creation_object) {
		current = MAlloc(sizeof(Symbol));
		current->name = StrNew(symbol_name);
		current->type = creation_object->type;
		MemCpy(&current->v, &creation_object->v, sizeof(NodeValue));
		VectorAdd(env->parent->symbols, current);
	}

	return current;
}

Bool IsTruthy(Object *obj) {
	if (obj->type == OBJ_NIL)
		return FALSE;
	if (obj->type == OBJ_INT)
		return TRUE;
	if (obj->type == OBJ_BOOLEAN)
		return obj->v.i;
	"IsTruthy: Unknown object type %x, returning false.\n", obj->type;
	return FALSE;
}

// Can't return more than 64-bits (pushed on stack).
Object *Eval(Node *root, Environment *parent_env = NULL)
{
	Object *ret = NULL, *left = NULL, *right = NULL;
	Node *node;
	Environment env;
	I64 i;

	EnvironmentNew(&env); // I could free the Vector in this but... eh. Memleaks ftw.
	env.parent = parent_env;

	if (root->children->len >= 2 && root->type != TOKEN_IF) {
		if (root->type != TOKEN_LET)
			left = Eval(&root->children->d[0], &env);
		right = Eval(&root->children->d[root->children->size], &env);
	}

	if (root->type == NODE_PROG) {
		for (i=0; i<root->children->len; i++) {
			ret = Eval(&root->children->d[root->children->size * i], &env);
		}
	} else if (root->type == TOKEN_INT) { // "Primitives", things that have values
		ret = ObjectNewI64(root->v.i);
	} else if (root->type == TOKEN_TRUE) {
		ret = ObjectNewBool(1);
	} else if (root->type == TOKEN_NIL) {
		ret = ObjectNewNil;
	} else if (root->type == TOKEN_IDENT) {
		ret = EnvironmentLookupAdd(&env, root->v.s);
		// FIXME: Handle error
		if (!ret)
			throw('novar');
	} else if (root->type == TOKEN_PLUS) { // Math operations
		if (!left || !right)
			throw('noLorR');
		if (left->type != OBJ_INT || right->type != OBJ_INT)
			throw('invLorR');
		ret = ObjectNewI64(left->v.i + right->v.i);
	} else if (root->type == TOKEN_DASH) {
		if (!left || !right)
			throw('noLorR');
		if (left->type != OBJ_INT || right->type != OBJ_INT)
			throw('invLorR');
		ret = ObjectNewI64(left->v.i - right->v.i);
	} else if (root->type == TOKEN_ASTERISK) {
		if (!left || !right)
			throw('noLorR');
		if (left->type != OBJ_INT || right->type != OBJ_INT)
			throw('invLorR');
		ret = ObjectNewI64(left->v.i * right->v.i);
	} else if (root->type == TOKEN_FORWARD_SLASH) {
		if (!left || !right)
			throw('noLorR');
		if (left->type != OBJ_INT || right->type != OBJ_INT)
			throw('invLorR');
		ret = ObjectNewI64(left->v.i / right->v.i);
	} else if (root->type == TOKEN_EQUAL) { // Logical operations
		if (!left || !right)
			throw('noLorR');
		if ((left->type == OBJ_INT && right->type == OBJ_INT) || (left->type == OBJ_BOOLEAN && right->type == OBJ_BOOLEAN))
			ret = ObjectNewBool(left->v.i == right->v.i);
		else if (left->type == OBJ_NIL && right->type == OBJ_NIL)
			ret = ObjectNewBool(TRUE);
		else
			ret = ObjectNewBool(FALSE);
	} else if (root->type == TOKEN_NOT_EQUAL) {
		if (!left || !right)
			throw('noLorR');
		if ((left->type == OBJ_INT && right->type == OBJ_INT) || (left->type == OBJ_BOOLEAN && right->type == OBJ_BOOLEAN))
			ret = ObjectNewBool(left->v.i != right->v.i);
		else if ((left->type == OBJ_NIL && right->type != OBJ_NIL) || (left->type != OBJ_NIL && right->type == OBJ_NIL))
			ret = ObjectNewBool(TRUE);
		else
			ret = ObjectNewBool(FALSE);
	} else if (root->type == TOKEN_LESS_THAN) {
		if (!left || !right)
			throw('noLorR');
		if (left->type != OBJ_INT || right->type != OBJ_INT)
			throw('invLorR');
		ret = ObjectNewBool(left->v.i < right->v.i);
	} else if (root->type == TOKEN_GREATER_THAN) {
		if (!left || !right)
			throw('noLorR');
		if (left->type != OBJ_INT || right->type != OBJ_INT)
			throw('invLorR');
		ret = ObjectNewBool(left->v.i > right->v.i);
	} else if (root->type == TOKEN_LESS_THAN_EQUAL) {
		if (!left || !right)
			throw('noLorR');
		if (left->type != OBJ_INT || right->type != OBJ_INT)
			throw('invLorR');
		ret = ObjectNewBool(left->v.i <= right->v.i);
	} else if (root->type == TOKEN_GREATER_THAN_EQUAL) {
		if (!left || !right)
			throw('noLorR');
		if (left->type != OBJ_INT || right->type != OBJ_INT)
			throw('invLorR');
		ret = ObjectNewBool(left->v.i >= right->v.i);
	} else if (root->type == TOKEN_LET) { // Var defs etc.
		if (!right)
			throw('noLorR');
		node = &root->children->d[0];
		if (node->type != TOKEN_IDENT)
			throw('invident');
		ret = EnvironmentLookupAdd(env.parent, node->v.s, right);
	} else if (root->type == TOKEN_IF) {
		if (root->children->len > 3 || root->children->len < 2)
			throw('invif');
		Bool truthy = IsTruthy(Eval(&root->children->d[0], env.parent));
		if (truthy)
			ret = Eval(&root->children->d[root->children->size], env.parent);
		else if (root->children->len == 3)
			ret = Eval(&root->children->d[root->children->size * 2], env.parent);
		else
			ret = ObjectNewNil;
	} else {
		// FIXME: Handle error
		throw('inveval');
	}

	return ret;
}


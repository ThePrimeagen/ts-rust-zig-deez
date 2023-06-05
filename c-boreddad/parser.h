#ifndef __PARSER_H__

#define __PARSER_H__

#include "token.h"
#include "lexer.h"
#include "ast.h"

typedef struct{
    Lexer* l;
    Token* cur;
    Token* peek;
}Parser;

extern Parser* parser_new(Lexer* l);
extern Program* parse_program(Parser* p);
extern size_t program_statements_len(Program* p);

#endif /*!__PARSER_H__*/

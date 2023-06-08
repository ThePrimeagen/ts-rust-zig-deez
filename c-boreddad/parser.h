#ifndef __PARSER_H__

#define __PARSER_H__

#include "token.h"
#include "lexer.h"
#include "ast.h"

typedef struct{
    size_t len;
    size_t cap;
    char** values;
}ParserErorrs;

typedef struct{
    Lexer* l;
    ParserErorrs* errors;
    Token* cur;
    Token* peek;
}Parser;

extern Parser* parser_new(Lexer* l);
extern Program* parse_program(Parser* p);
extern char** parser_errors(Parser* p);
extern size_t program_statements_len(Program* p);
extern size_t parser_errors_length(Parser* p);

#endif /*!__PARSER_H__*/

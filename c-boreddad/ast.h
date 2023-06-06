#ifndef __AST_H__

#define __AST_H__

#include "token.h"
#include "lexer.h"

typedef struct{
    Token* tok;
    char* value;
}Identifier;

typedef struct{
    Token* tok;
    Identifier* name;
}LetStatement;

typedef enum{
    LETSTATEMENT,
    NULLSTATEMENT
}StatementType;

typedef struct{
    StatementType type;
    void* value;
}Statement;

typedef struct{
    size_t len;
    size_t cap;
    Statement** values;
}Statements;

typedef struct{
    Statements* statements;
}Program;

#endif /*!__AST_H__*/

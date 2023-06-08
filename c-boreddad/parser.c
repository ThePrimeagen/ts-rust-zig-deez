#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "token.h"
#include "lexer.h"
#include "ast.h"
#include "parser.h"

void parser_peek_error(Parser* p, TokenT type);

static void parser_next_token(Parser* p)
{
    p->cur = p->peek;
    p->peek = lexer_next_token(p->l);
}

static ParserErorrs* init_errors(size_t initial_cap)
{
    ParserErorrs* errors;

    errors = calloc(1, sizeof *errors);
    if (errors == NULL)
    {
        return NULL;
    }

    errors->values = calloc(initial_cap, sizeof(char*));
    if (errors->values == NULL)
    {
        free(errors);
        return NULL;
    }

    errors->cap = initial_cap;

    return errors;
}

static Statements* create_statemets(size_t initial_cap)
{
    Statements* ss;

    ss = calloc(1, sizeof *ss);
    if (ss == NULL)
    {
        return NULL;
    }

    ss->values = calloc(initial_cap, sizeof(void*));
    if (ss->values == NULL)
    {
        free(ss);
        return NULL;
    }

    ss->cap = initial_cap;

    return ss;
}

static Identifier* identifier_new(Token* tok)
{
    Identifier* ident;

    ident = calloc(1, sizeof *ident);
    if (ident == NULL)
    {
        return NULL;
    }

    ident->tok = tok;
    ident->value = tok->literal;

    return ident;
}

static uint8_t parser_cur_token_is(Parser* p, TokenT type)
{
    return p->cur->type == type ? 1 : 0;
}

static uint8_t peek_token_is(Parser* p, TokenT type)
{
    return p->peek->type == type ? 1 : 0;
}

static uint8_t parser_expect_peek(Parser* p, TokenT type)
{
    if (peek_token_is(p, type))
    {
        parser_next_token(p);
        return 1;
    }
    parser_peek_error(p, type);
    return 0;
}

static char* format_error_str(Parser* p, TokenT type)
{
    char* error_str;
    char *exp_token_type_str, *peek_token_type_str;
    size_t size, exp_token_type_str_size, peek_token_type_str_size;

    exp_token_type_str = get_token_type_str(type);
    peek_token_type_str = get_token_type_str(p->peek->type);

    exp_token_type_str_size = strlen(exp_token_type_str);
    peek_token_type_str_size = strlen(peek_token_type_str);

    size = 41 + exp_token_type_str_size + peek_token_type_str_size + 1;

    error_str = calloc(size, sizeof *error_str);

    if (error_str == NULL)
    {
        return NULL;
    }

    strncpy(error_str, "expected next token to be ", 27);
    strncat(error_str, exp_token_type_str, exp_token_type_str_size);
    strncat(error_str, "s, got ", 8);
    strncat(error_str, peek_token_type_str, peek_token_type_str_size);
    strncat(error_str, " instead", 9);

    return error_str;
}

void append_errors(ParserErorrs* pe, char* e)
{
    if (pe->len == pe->cap)
    {
        pe->cap += pe->cap;
        pe->values = realloc(pe->values, sizeof(char*) * pe->cap);
        memset(pe->values + pe->len, 0, pe->cap - pe->len);
    }

    pe->values[pe->len] = e;
    pe->len++;
}

void parser_peek_error(Parser* p, TokenT type)
{
    char* error_str;

    error_str = format_error_str(p, type);
    if (error_str == NULL)
    {
        return;
    }
    append_errors(p->errors, error_str);
}

static LetStatement* parser_parse_let_statement(Parser* p)
{
    LetStatement* stmt;

    stmt = calloc(1, sizeof *stmt);
    if (stmt == NULL)
    {
        return NULL;
    }

    stmt->tok = p->cur;

    if (!parser_expect_peek(p, IDENT))
    {
        free(stmt);
        return NULL;
    }

    stmt->name = identifier_new(p->cur);
    if (stmt->name == NULL)
    {
        free(stmt);
        return NULL;
    }

    if (!parser_expect_peek(p, ASSIGN))
    {
        free(stmt->name);
        free(stmt);
        return NULL;
    }

    /*! TODO: we're skipping expressions */

    for (; parser_cur_token_is(p, SEMICOLON); parser_next_token(p))
    {
    }

    return stmt;
}

static Statement* parser_parse_statement(Parser* p)
{
    Statement* s;

    s = calloc(1, sizeof *s);
    if (s == NULL)
    {
        return NULL;
    }

    if (p->cur->type == LET)
    {
        s->value = parser_parse_let_statement(p);
        if (s->value == NULL)
        {
            free(s);
            return NULL;
        }
        s->type = LETSTATEMENT;
        return s;
    }

    free(s);

    return NULL;
}

static void append_statements(Statements* ss, Statement* s)
{
    if (ss->len == ss->cap)
    {
        ss->cap += ss->cap;
        ss->values = realloc(ss->values, (sizeof(Statement*)) * ss->cap);
        memset(ss->values + ss->len, 0, ss->cap - ss->len);
    }

    ss->values[ss->len] = s;
    ss->len++;
}

Parser* parser_new(Lexer* l)
{
    Parser* p;

    p = calloc(1, sizeof *p);
    if (p == NULL)
    {
        return NULL;
    }

    p->errors = init_errors(32);
    if (p->errors == NULL)
    {
        free(p);
        return NULL;
    }

    p->l = l;

    parser_next_token(p);
    parser_next_token(p);
    return p;
}

Program* parse_program(Parser* p)
{
    Program* program;

    program = calloc(1, sizeof *program);
    if (program == NULL)
    {
        return NULL;
    }

    program->statements = create_statemets(32);
    if (program->statements == NULL)
    {
        free(program);
        return NULL;
    }

    for (; p->cur->type != EOFT; parser_next_token(p))
    {
        Statement* stmt = parser_parse_statement(p);
        if (stmt != NULL)
        {
            append_statements(program->statements, stmt);
        }
    }
    return program;
}

size_t program_statements_len(Program* p)
{
    if (p == NULL)
    {
        return 0;
    }
    if (p->statements == NULL)
    {
        return 0;
    }
    return p->statements->len;
}

char** parser_errors(Parser* p)
{
    return p->errors->values;
}

size_t parser_errors_length(Parser* p)
{
    return p->errors->len;
}

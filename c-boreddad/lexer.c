#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "token.h"
#include "lexer.h"

void lexer_read_char(Lexer* l);

Lexer* lexer_new(char* input)
{
    Lexer* l;
    l = calloc(1, sizeof *l);
    if (l == NULL)
    {
        return NULL;
    }

    l->input = input;
    l->input_len = strlen(input);

    lexer_read_char(l);
    return l;
}

uint8_t is_letter(char ch)
{
    return ((('a' <= ch) && (ch <= 'z')) || (('A' <= ch) && (ch <= 'Z')) || (ch == '_')) ? 1 : 0;
}

char* read_identifier(Lexer* l)
{
    size_t pos;
    uint8_t is;
    char* ret;
    pos = l->position;
    while ((is = is_letter(l->ch)))
    {
        lexer_read_char(l);
        is = is_letter(l->ch);
    }
    ret = calloc((l->position - pos) + 1, sizeof *ret);
    if (ret == NULL)
    {
        return NULL;
    }
    memcpy(ret, l->input + pos, l->position - pos);
    /* have to subtract cause while loop */
    l->position--;
    l->read_position--;
    return ret;
}

uint8_t is_digit(char ch)
{
    return (('0' <= ch) && (ch <= '9')) ? 1 : 0;
}

char* read_number(Lexer* l)
{
    size_t pos;
    uint8_t is;
    char* ret;
    pos = l->position;
    while ((is = is_digit(l->ch)))
    {
        lexer_read_char(l);
        is = is_digit(l->ch);
    }
    ret = calloc((l->position - pos) + 1, sizeof *ret);
    if (ret == NULL)
    {
        return NULL;
    }
    memcpy(ret, l->input + pos, l->position - pos);
    /* have to subtract cause while loop */
    l->position--;
    l->read_position--;
    return ret;
}

void lexer_read_char(Lexer* l)
{
    size_t len;
    len = l->input_len;
    if (l->read_position >= len)
    {
        l->ch = 0;
    }
    else
    {
        l->ch = l->input[l->read_position];
    }

    l->position = l->read_position;
    l->read_position += 1;
}

void skip_white_space(Lexer* l)
{
    while ((l->ch == ' ') || (l->ch == '\t') || (l->ch == '\n') || (l->ch == '\r'))
    {
        lexer_read_char(l);
    }
}

char peek_char(Lexer* l)
{
    if (l->read_position >= l->input_len)
    {
        return 0;
    }
    return l->input[l->position];
}

char* get_token_type_str(TokenT t)
{
    switch (t) {
        case ILLEGAL: return "ILLEGAL";
        case EOFT: return "EOFT";
        case IDENT: return "IDENT";
        case INT: return "INT";
        case ASSIGN: return "ASSIGN";
        case PLUS: return "PLUS";
        case MINUS: return "MINUS";
        case BANG: return "BANG";
        case ASTERIK: return "ASTERIK";
        case SLASH: return "SLASH";
        case LT: return "LT";
        case GT: return "GT";
        case COMMA: return "COMMA";
        case SEMICOLON: return "SEMICOLON";
        case LPAREN: return "LPAREN";
        case RPAREN: return "RPAREN";
        case LSQUIRLY: return "LSQUIRLY";
        case RSQUIRLY: return "RSQUIRLY";
        case FUNCTION: return "FUNCTION";
        case LET: return "LET";
        case TRUE: return "TRUE";
        case FALSE: return "FALSE";
        case IF: return "IF";
        case ELSE: return "ELSE";
        case RETURN: return "RETURN";
        case EQ: return "EQ";
        case NOT_EQ: return "NOT_EQ";
    }
    return "";
}

Token* lexer_next_token(Lexer* l)
{
    Token* tok;
    tok = calloc(1, sizeof *tok);
    skip_white_space(l);
    if (tok == NULL)
    {
        return NULL;
    }

    if (l->ch == '=')
    {
        lexer_read_char(l);
        if (peek_char(l) == '=')
        {
            tok->type = EQ;
            tok->literal = "==";
            lexer_read_char(l);
            return tok;
        }
        tok->type = ASSIGN;
        tok->literal = "=";
        return tok;
    }
    if (l->ch == ';')
    {
        tok->type = SEMICOLON;
        tok->literal = ";";
        lexer_read_char(l);
        return tok;
    }
    if (l->ch == '(')
    {
        tok->type = LPAREN;
        tok->literal = "(";
        lexer_read_char(l);
        return tok;
    }
    if (l->ch == ')')
    {
        tok->type = RPAREN;
        tok->literal = ")";
        lexer_read_char(l);
        return tok;
    }
    if (l->ch == ',')
    {
        tok->type = COMMA;
        tok->literal = ",";
        lexer_read_char(l);
        return tok;
    }
    if (l->ch == '+')
    {
        tok->type = PLUS;
        tok->literal = "+";
        lexer_read_char(l);
        return tok;
    }
    if (l->ch == '-')
    {
        tok->type = MINUS;
        tok->literal = "-";
        lexer_read_char(l);
        return tok;
    }
    if (l->ch == '!')
    {
        lexer_read_char(l);
        if (peek_char(l) == '=')
        {
            tok->type = NOT_EQ;
            tok->literal = "!=";
            lexer_read_char(l);
        }
        tok->type = BANG;
        tok->literal = "!";
        return tok;
    }
    if (l->ch == '*')
    {
        tok->type = ASTERIK;
        tok->literal = "*";
        lexer_read_char(l);
        return tok;
    }
    if (l->ch == '/')
    {
        tok->type = SLASH;
        tok->literal = "/";
        lexer_read_char(l);
        return tok;
    }
    if (l->ch == '<')
    {
        tok->type = LT;
        tok->literal = "<";
        lexer_read_char(l);
        return tok;
    }
    if (l->ch == '>')
    {
        tok->type = GT;
        tok->literal = ">";
        lexer_read_char(l);
        return tok;
    }
    if (l->ch == '{')
    {
        tok->type = LSQUIRLY;
        tok->literal = "{";
        lexer_read_char(l);
        return tok;
    }
    if (l->ch == '}')
    {
        tok->type = RSQUIRLY;
        tok->literal = "}";
        lexer_read_char(l);
        return tok;
    }
    if ((l->ch == 0) || (l->position > l->input_len))
    {
        tok->literal = "";
        tok->type = EOFT;
        lexer_read_char(l);
        return tok;
    }
    if (is_letter(l->ch))
    {
        tok->literal = read_identifier(l);
        tok->type = lookup_ident(tok->literal);
        lexer_read_char(l);
        return tok;
    }
    else if (is_digit(l->ch))
    {
        tok->type = INT;
        tok->literal = read_number(l);
        lexer_read_char(l);
        return tok;
    }
    else
    {
        tok->type = ILLEGAL;
        tok->literal = (char*)&(l->ch);
        lexer_read_char(l);
        return tok;
    }
    tok->literal = "";
    tok->type = EOFT;
    lexer_read_char(l);
    return tok;
}

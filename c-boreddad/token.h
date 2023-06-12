#ifndef __TOKEN_H__

#define __TOKEN_H__

typedef enum{
    ILLEGAL,
    EOFT,
    IDENT,
    INT,
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERIK,
    SLASH,
    LT,
    GT,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LSQUIRLY,
    RSQUIRLY,
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
    EQ,
    NOT_EQ
}TokenT;

typedef struct {
    TokenT type;
    char* literal;
}Token;

TokenT lookup_ident(char* ident);
extern void free_token(Token* tok);

#endif /*!__TOKEN_H__*/

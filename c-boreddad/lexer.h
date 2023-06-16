#ifndef __LEXER_H__

#define __LEXER_H__

#include <stddef.h>
#include <stdint.h>
#include "token.h"

typedef struct{
    char* input;
    size_t input_len;
    size_t position;
    size_t read_position;
    uint8_t ch;
}Lexer;

extern Lexer* lexer_new(char* input);
extern Token* lexer_next_token(Lexer* l);
extern char* get_token_type_str(TokenT t);

#endif /*!__LEXER_H__*/

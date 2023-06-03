#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include "token.h"

#define KEY_WORDS_LEN 7
char* const key_words[] = {"fn", "let", "true", "false", "if", "else", "return"};

TokenT lookup_ident(char* ident)
{
    size_t i, ident_len;
    ident_len = strlen(ident);
    for (i = 0; i < KEY_WORDS_LEN; i++)
    {
        char* key_word;
        size_t key_word_len;
        key_word = key_words[i];
        key_word_len = strlen(key_word);
        if ((key_word_len == ident_len) && (strncmp(ident, key_word, ident_len) == 0))
        {
            break;
        }
    }
    if (i == 0)
    {
        return FUNCTION;
    }
    if (i == 1)
    {
        return LET;
    }
    if (i == 2)
    {
        return TRUE;
    }
    if (i == 3)
    {
        return FALSE;
    }
    if (i == 4)
    {
        return IF;
    }
    if (i == 5)
    {
        return ELSE;
    }
    if (i == 6)
    {
        return RETURN;
    }
    return IDENT;
}

void free_token(Token* tok)
{
    if ((tok->type == IDENT) || (tok->type == FUNCTION)
            || (tok->type == INT) || (tok->type == LET)
            || (tok->type == TRUE) || (tok->type == FALSE)
            || (tok->type == IF) || (tok->type == ELSE)
            || (tok->type == RETURN))
    {
        free(tok->literal);
    }
    free(tok);
}

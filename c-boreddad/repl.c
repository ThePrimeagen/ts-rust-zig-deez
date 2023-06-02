#include "lexer.h"
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <stddef.h>
#include <stdlib.h>

ssize_t get_line(char** str, size_t* len);

#define ROUTINE                                             \
    fprintf(stdout, ">> ");                                 \
    fflush(stdout);

void start()
{
    char* line = NULL;
    size_t line_len = 0;
    ssize_t gl_res;
    Lexer* l;
    Token* tok;

    ROUTINE;

    /* c89 doesn't have getline :( */
    gl_res = get_line(&line, &line_len);

    if (gl_res == -1)
    {
        if (line)
        {
            free(line);
        }
        return;
    }

    if (!line)
    {
        return;
    }

    l = lexer_new(line);

    for (tok = lexer_next_token(l); tok->type != EOFT; tok = lexer_next_token(l))
    {
        printf("type: %u literal: %s\n", tok->type, tok->literal);
        if ((tok->type == IDENT) || (tok->type == FUNCTION) || (tok->type == INT)
                || (tok->type == TRUE) || (tok->type == FALSE)
                || (tok->type == IF) || (tok->type == ELSE)
                || (tok->type == RETURN))
        {
            free(tok->literal);
        }
        free(tok);
    }

    free(line);
}

ssize_t get_line(char** str, size_t* len)
{
    char c;
    size_t capacity;
    capacity = 120;
    if (!(*str))
    {
        *str = calloc(capacity, sizeof **str);
        if (*str == NULL)
        {
            return -1;
        }
    }
    for (*len = 0; (c = getc(stdin)) != '\n'; (*len)++)
    {
        if (*len == capacity)
        {
            capacity += 60;
            *str = realloc(*str, (sizeof **str) * capacity);
            if (*str == NULL)
            {
                goto err;
            }
            memset((*str) + (*len), 0, (capacity - (*len)));
        }
        *((*str) + (*len)) = c;
    }
    return ((ssize_t)(capacity));

err:
    if (*str) free(*str);
    *len = 0;
    return -1;
}



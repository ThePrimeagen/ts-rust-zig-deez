#include "lexer.h"
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <stddef.h>
#include <stdlib.h>

ssize_t get_line(char** str, size_t* len);
int run_repl();

#define ROUTINE                                             \
    fprintf(stdout, ">> ");                                 \
    fflush(stdout);

void start()
{
    for (;;)
    {
        if (run_repl() != 0)
        {
            break;
        }
    }
}

int run_repl()
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
        return -1;
    }

    if (!line)
    {
        return -1;
    }

    if (strncmp(line, "exit", 4) == 0)
    {
        free(line);
        return 1;
    }

    l = lexer_new(line);

    for (tok = lexer_next_token(l); tok->type != EOFT; tok = lexer_next_token(l))
    {
        printf("type: %u literal: %s\n", tok->type, tok->literal);
        free_token(tok);
    }

    free_token(tok);
    free(l);
    free(line);
    return 0;
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



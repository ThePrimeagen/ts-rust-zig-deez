#!/usr/bin/env bash

_lex_remove_leading_whitespace() {
    declare -n ret=$1
    ret="${ret#"${ret%%[![:space:]]*}"}"
}

_lex_chomp() {
    declare -n ret=$1
    local needle=$2
    if [[ "${ret:0:${#needle}}" == "$needle" ]]; then
        ret="${ret:${#needle}}"
        return 0
    else
        return 1
    fi
}

tokenize() {
    local src=$1

    while [[ -n "$src" ]]; do
        _lex_remove_leading_whitespace src

        if _lex_chomp src "let"; then echo "TOK_LET;"
        elif _lex_chomp src "fn"; then echo "TOK_FN;"
        elif [[ "$src" =~ ^[a-zA-Z_][a-zA-Z0-9_]* ]]; then
            local token; token="${BASH_REMATCH[0]}"
            echo "TOK_IDENT;$token"
            src="${src:${#token}}"
        elif [[ "$src" =~ ^[0-9]+ ]]; then
            local token; token="${BASH_REMATCH[0]}"
            echo "TOK_INT;$token"
            src="${src:${#token}}"
        elif _lex_chomp src "="; then echo "TOK_EQ;"
        elif _lex_chomp src "+"; then echo "TOK_PLUS;"
        elif _lex_chomp src ","; then echo "TOK_COMMA;"
        elif _lex_chomp src ";"; then echo "TOK_SEMICOLON;"
        elif _lex_chomp src "("; then echo "TOK_LPAREN;"
        elif _lex_chomp src ")"; then echo "TOK_RPAREN;"
        elif _lex_chomp src "{"; then echo "TOK_LSQUIRLY;"
        elif _lex_chomp src "}"; then echo "TOK_RSQUIRLY;"
        else
            echo "ILLEGAL;${src:0:10}"
            return
        fi
    done

    echo "TOK_EOF;"
}

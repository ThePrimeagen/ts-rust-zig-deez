! Copyright (C) 2023 Your name.
! See https://factorcode.org/license.txt for BSD license.
USING: kernel sequences io prettyprint deez.lexer ;
IN: deez.repl

CONSTANT: PROMPT ">>"

: repl-input ( -- input ) PROMPT write readln ; 

: print-tokens ( seq -- ) 
    [ dup empty? not ] [ dup first . rest ] while drop ;

: repl ( -- )
    [
        repl-input dup empty? not
        [ <lexer> lex print-tokens t ]
        [ drop f ] if
    ] loop ;

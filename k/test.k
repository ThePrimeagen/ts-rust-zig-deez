\l lexer.k
/ my testing framework of choice is ~
$[(`let;(`ident;,"a");`equal;`lsb;(`ident;,"b");`comma;(`ident;,"c");`rsb)~lexer.lex "let a=[b,c]";"yay";`err@"test"]

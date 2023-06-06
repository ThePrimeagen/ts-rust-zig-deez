/ degen comments are annotated by DC
lexer.classify:{a:-1<y?x
                a[&-1=-':a]:("\0"\`c$x*a)^,"" / DC a true samurai has no position, only order
                a}
lexer.symbols:{t:("let";,"=";,"[";,"]";,",")!(`let`equal`lsb`rsb`comma)
               $[~`C~@x;0;r:t@x;r;*r:t@,'x;r;,(`ident;x)]} / DC/non-DC the only thing that is array-programming about this is that the conditional is vectorized
lexer.lex:{x:x," ";,/(lexer.symbols'`c$lexer.classify[x;"qwertyuiopasdfghjklzxcvbnm"]|lexer.classify[x;"=[],"])^,0}

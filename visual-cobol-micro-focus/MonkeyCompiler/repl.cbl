       class-id MonkeyCompiler.repl static.

       working-storage section.
       

       method-id RunREPL(cmd-prompt as string) returning z as condition-value static.
       local-storage section.
       01 scan-line string.
       01 my-lex type MonkeyCompiler.lib.Lexer.
       01 lexed-token type MonkeyCompiler.lib.Token.
       procedure division.

           display cmd-prompt with no advancing.
           accept scan-line.

           set my-lex to new type MonkeyCompiler.lib.Lexer(scan-line).
           set lexed-token to my-lex::next-token.
           display "(" & lexed-token::TokenClass & ", " & lexed-token::TokenLiteral & ")".

           perform until lexed-token::TokenClass = type MonkeyCompiler.lib.TokenType::EOF
               set lexed-token to my-lex::next-token
               display "(" & lexed-token::TokenClass & ", " & lexed-token::TokenLiteral & ")"
           end-perform.
       end method.

       end class.

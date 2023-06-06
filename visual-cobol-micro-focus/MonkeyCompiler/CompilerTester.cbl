       class-id. CompilerTester.

       method-id main public static.
       local-storage section.
       01 test-input string.
       01 test-lexer type MonkeyCompiler.lib.Lexer.
       01 test-assert-tokens type MonkeyCompiler.lib.Token occurs 37 times.
       01 test-lexed-token type MonkeyCompiler.lib.Token.
       01 test-lexed-tokens type System.Collections.ArrayList.
       procedure division.

           display "Entered Program.".
           set test-lexed-tokens to new System.Collections.ArrayList.

           set test-input to
             "let five = 5;" & x"0a" &
             "let ten = 10;" & x"0a" &
             x"0a" &
             "let add = fn(x, y) {" & x"0a" &
             "  x + y;" & x"0a" &
             "};" & x"0a" &
             x"0a" &
             "let result = add(five, ten);" & x"0a".

           display "Test input: " & x"0a" & test-input.

           set test-assert-tokens[0] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::LET, "let").
           set test-assert-tokens[1] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::IDENT, "five").
           set test-assert-tokens[2] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::ASSIGN, "=").
           set test-assert-tokens[3] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::INT, "5").
           set test-assert-tokens[4] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::SEMICOLON, ";").
           set test-assert-tokens[5] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::LET, "let").
           set test-assert-tokens[6] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::IDENT, "ten").
           set test-assert-tokens[7] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::ASSIGN, "=").
           set test-assert-tokens[8] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::INT, "10").
           set test-assert-tokens[9] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::SEMICOLON, ";").
           set test-assert-tokens[10] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::LET, "let").
           set test-assert-tokens[11] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::IDENT, "add").
           set test-assert-tokens[12] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::ASSIGN, "=").
           set test-assert-tokens[13] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::FUNCTION, "fn").
           set test-assert-tokens[14] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::LPAREN, "(").
           set test-assert-tokens[15] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::IDENT, "x").
           set test-assert-tokens[16] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::COMMA, ",").
           set test-assert-tokens[17] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::IDENT, "y").
           set test-assert-tokens[18] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::RPAREN, ")").
           set test-assert-tokens[19] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::LBRACE, "{").
           set test-assert-tokens[20] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::IDENT, "x").
           set test-assert-tokens[21] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::PLUS, "+").
           set test-assert-tokens[22] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::IDENT, "y").
           set test-assert-tokens[23] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::SEMICOLON, ";").
           set test-assert-tokens[24] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::RBRACE, "}").
           set test-assert-tokens[25] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::SEMICOLON, ";").
           set test-assert-tokens[26] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::LET, "let").
           set test-assert-tokens[27] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::IDENT, "result").
           set test-assert-tokens[28] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::ASSIGN, "=").
           set test-assert-tokens[29] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::IDENT, "add").
           set test-assert-tokens[30] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::LPAREN, "(").
           set test-assert-tokens[31] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::IDENT, "five").
           set test-assert-tokens[32] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::COMMA, ",").
           set test-assert-tokens[33] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::IDENT, "ten").
           set test-assert-tokens[34] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::RPAREN, ")").
           set test-assert-tokens[35] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::SEMICOLON, ";").
           set test-assert-tokens[36] to new type MonkeyCompiler.lib.Token(type MonkeyCompiler.lib.TokenType::EOF, "").

           display "Constructing Lexer".
           set test-lexer to new type MonkeyCompiler.lib.Lexer(test-input).

           display "Constructing lexed token list".
           set test-lexed-token to test-lexer::next-token.
           display "(" & test-lexed-token::TokenClass & ", " & test-lexed-token::TokenLiteral & ")".

           perform until test-lexed-token::TokenClass = type MonkeyCompiler.lib.TokenType::EOF
               invoke test-lexed-tokens::Add(test-lexed-token)
               set test-lexed-token to test-lexer::next-token
               display "(" & test-lexed-token::TokenClass & ", " & test-lexed-token::TokenLiteral & ")"
           end-perform.

           invoke test-lexed-tokens::Add(test-lexed-token).
           display "Lexed token list built. Beginning tests.".

           if size of test-assert-tokens = test-lexed-tokens::Count
               display "Expected and Lexed token counts match. Continuing to test of equality."
           else
               display "Expected and Lexed token counts do not match. Aborting test with Failing result."
               stop run
           end-if.

           declare my-index as binary-long = 0.

           perform until my-index >= test-lexed-tokens::Count
               if not test-assert-tokens[my-index]::is-equal-to(test-lexed-tokens[my-index] as type MonkeyCompiler.lib.Token)
                   display "Lexed token mismatch at token stream position " & my-index & ". Aborting test."
                   stop run
               end-if
               set my-index to my-index + 1
           end-perform.

           display "All tests completed without error.".

           declare temp as string.
           accept temp.

       end method.

       end class.

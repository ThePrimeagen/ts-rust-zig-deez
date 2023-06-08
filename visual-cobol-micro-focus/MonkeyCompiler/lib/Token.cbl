       class-id MonkeyCompiler.lib.Token.

       working-storage section.
       01 token-class type MonkeyCompiler.lib.TokenType private.
       01 token-literal string private.

       property-id TokenClass type MonkeyCompiler.lib.TokenType.
           getter.
               set property-value to token-class.
       end property.

       property-id TokenLiteral string.
           getter.
               set property-value to token-literal.
       end property.

       method-id new(tokenClass as type MonkeyCompiler.lib.TokenType, 
                     tokenLit as string).
       local-storage section.
       procedure division.
           set token-class to tokenClass.
           set token-literal to tokenLit.
       end method.

       method-id lookup-ident (arg as string) returning z as type MonkeyCompiler.lib.TokenType static.
       procedure division
           evaluate arg
               when "fn"
                   set z to type MonkeyCompiler.lib.TokenType::FUNCTION
               when "let"
                   set z to type MonkeyCompiler.lib.TokenType::LET
               when other
                   set z to type MonkeyCompiler.lib.TokenType::IDENT
           end-evaluate.
       end method.

       method-id. is-equal-to (other-token as type MonkeyCompiler.lib.Token) returning z as condition-value.
       local-storage division.
       01 token-type-equal condition-value.
       01 token-literal-eq condition-value.
       procedure division.
           if self::token-class = other-token::TokenClass
               set token-type-equal to true
           end-if.

           if self::token-literal = other-token::TokenLiteral
               set token-literal-eq to true
           end-if.

           if token-type-equal and token-literal-eq
               set z to true
           else
               set z to false
           end-if.
       end method.

       end class.

       enum-id MonkeyCompiler.lib.TokenType.
       78 #ILLEGAL.
       78 #EOF.
       78 #IDENT.
       78 #INT.
       78 #ASSIGN.
       78 #PLUS.
       78 #COMMA.
       78 #SEMICOLON.
       78 #LPAREN.
       78 #RPAREN.
       78 #LBRACE.
       78 #RBRACE.
       78 #FUNCTION.
       78 #LET.
       end enum.
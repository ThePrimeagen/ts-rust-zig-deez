       class-id MonkeyCompiler.lib.Lexer.

       working-storage section.
       01 input-string string.
       01 pos          binary-long.
       01 read-pos     binary-long.
       01 cur-ch       character.

       method-id new(in-string as string).
       procedure division.
           set input-string to in-string.
           set pos to 0.
           set read-pos to 0.
           invoke self::read-char.
       end method.

       method-id read-char.
       procedure division.
           if read-pos >= input-string::Length
               set cur-ch to 0
           else
               set cur-ch to input-string[read-pos]
           end-if.
           set pos to read-pos.
           set read-pos to read-pos + 1.
       end method.

       method-id is-letter (x as character)
           returning z as condition-value.
       procedure division.
           if type System.Char::IsLetter(cur-ch) or cur-ch = "_"
               set z to true
           else
               set z to false
           end-if.
       end method.

       method-id is-number (x as character)
           returning z as condition-value.
       procedure division.
           if type System.Char::IsDigit(x)
               set z to true
           else
               set z to false
           end-if.
       end method.

       method-id is-whitespace (x as character)
           returning z as condition-value.
       procedure division.
           if type System.Char::IsWhiteSpace(x)
               set z to true
           else
               set z to false
           end-if.
       end method.

       method-id read-ident
           returning z as string.
       local-storage section.
       01 my-pos binary-long.
       procedure division.
           set my-pos to pos.

           perform until not self::is-letter(cur-ch)
               invoke self::read-char
           end-perform.

           declare my-len as binary-long = pos - my-pos.

           set z to input-string::Substring(my-pos, my-len).
           set pos to pos - 1.
           set read-pos to read-pos - 1.
       end method.

       method-id read-num
           returning z as string.
       local-storage section.
       01 my-pos binary-long.
       procedure division.
           set my-pos to pos.

           perform until not self::is-number(cur-ch)
               invoke self::read-char
           end-perform.

           declare my-len as binary-long = pos - my-pos.

           set z to input-string::Substring(my-pos, my-len).
           set pos to pos - 1.
           set read-pos to read-pos - 1.
       end method.

       method-id skip-whitespace.
       procedure division.
           if not self::is-whitespace(cur-ch)
               exit method
           end-if.

           perform until not self::is-whitespace(cur-ch)
               invoke self::read-char
           end-perform.
       end method.

       method-id next-token
           returning tok-z as type MonkeyCompiler.lib.Token.
       local-storage section.
       01 tok type MonkeyCompiler.lib.Token.
       01 is-letter-var condition-value.
       01 is-number-var condition-value.
       procedure division.
           invoke self::skip-whitespace.

           set is-letter-var to self::is-letter(cur-ch).
           set is-number-var to self::is-number(cur-ch).
           evaluate cur-ch
               when "="
                   set tok to new MonkeyCompiler.lib.Token(type TokenType::ASSIGN, "=")
               when ";"
                   set tok to new MonkeyCompiler.lib.Token(type TokenType::SEMICOLON, ";")
               when "("
                   set tok to new MonkeyCompiler.lib.Token(type TokenType::LPAREN, "(")
               when ")"
                   set tok to new MonkeyCompiler.lib.Token(type TokenType::RPAREN, ")")
               when ","
                   set tok to new MonkeyCompiler.lib.Token(type TokenType::COMMA, ",")
               when "+"
                   set tok to new MonkeyCompiler.lib.Token(type TokenType::PLUS, "+")
               when "{"
                   set tok to new MonkeyCompiler.lib.Token(type TokenType::LBRACE, "{")
               when "}"
                   set tok to new MonkeyCompiler.lib.Token(type TokenType::RBRACE, "}")
               when 0
                   set tok to new MonkeyCompiler.lib.Token(type TokenType::EOF, "")
               when other
                   if is-letter-var
                       declare temp-literal as string = self::read-ident
                       declare temp-type as type MonkeyCompiler.lib.TokenType
                       set temp-type to type MonkeyCompiler.lib.Token::lookup-ident(temp-literal)
                       set tok to new MonkeyCompiler.lib.Token(temp-type, temp-literal)
                   else
                       if is-number-var
                           declare temp-literal as string = self::read-num
                           declare temp-type as type MonkeyCompiler.lib.TokenType = type MonkeyCompiler.lib.TokenType::INT
                           set tok to new MonkeyCompiler.lib.Token(temp-type, temp-literal)
                       else
                           set tok to new MonkeyCompiler.lib.Token(type TokenType::ILLEGAL, input-string[read-pos:1])
                       end-if
                   end-if
           end-evaluate.

           invoke self::read-char.
           set tok-z to tok.
       end method.

       end class.
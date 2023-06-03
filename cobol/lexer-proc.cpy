       LEXER-INIT-PROCEDURE.
           SET LEXER-OUTPUT-TOKEN-TYPE-LIST-COUNT TO 0

           PERFORM VARYING WS-LEXER-LINE-IDX FROM 1 BY 1 UNTIL
               WS-LEXER-LINE-IDX > LENGTH OF LEXER-INPUT-LINES

               MOVE SPACES TO LEXER-INPUT-LINES (WS-LEXER-LINE-IDX)
           END-PERFORM.

       LEXER-PARSE-PROCEDURE.
      * note that there can still be garbage in LEXER-OUTPUT-TOKEN-TYPE-LIST
           PERFORM VARYING WS-LEXER-LINE-IDX FROM 1 BY 1 UNTIL
               WS-LEXER-LINE-IDX > LENGTH OF LEXER-INPUT-LINES

               MOVE LEXER-INPUT-LINES (WS-LEXER-LINE-IDX) TO
               WS-LEXER-LINE

      * yes, we are looping through all 1024 * 256 chars
               PERFORM VARYING WS-LEXER-CH-IDX FROM 1 BY 1 UNTIL
                   WS-LEXER-CH-IDX > LENGTH OF WS-LEXER-LINE

                   MOVE WS-LEXER-LINE(WS-LEXER-CH-IDX:1) TO WS-LEXER-CH

                   EVALUATE WS-LEXER-CH
                       WHEN " "
                           CONTINUE
                       WHEN "="
                           ADD 1 TO LEXER-OUTPUT-TOKEN-TYPE-LIST-COUNT

                           SET LEXER-OUTPUT-TOKEN-TYPE-LIST
                           (LEXER-OUTPUT-TOKEN-TYPE-LIST-COUNT) TO
                           TOKEN-TYPE-ASSIGN
                       WHEN "+"
                           ADD 1 TO LEXER-OUTPUT-TOKEN-TYPE-LIST-COUNT

                           SET LEXER-OUTPUT-TOKEN-TYPE-LIST
                           (LEXER-OUTPUT-TOKEN-TYPE-LIST-COUNT) TO
                           TOKEN-TYPE-PLUS
                       WHEN ","
                           ADD 1 TO LEXER-OUTPUT-TOKEN-TYPE-LIST-COUNT

                           SET LEXER-OUTPUT-TOKEN-TYPE-LIST
                           (LEXER-OUTPUT-TOKEN-TYPE-LIST-COUNT) TO
                           TOKEN-TYPE-COMMA
                       WHEN ";"
                           ADD 1 TO LEXER-OUTPUT-TOKEN-TYPE-LIST-COUNT

                           SET LEXER-OUTPUT-TOKEN-TYPE-LIST
                           (LEXER-OUTPUT-TOKEN-TYPE-LIST-COUNT) TO
                           TOKEN-TYPE-SEMICOLON
                       WHEN "("
                           ADD 1 TO LEXER-OUTPUT-TOKEN-TYPE-LIST-COUNT

                           SET LEXER-OUTPUT-TOKEN-TYPE-LIST
                           (LEXER-OUTPUT-TOKEN-TYPE-LIST-COUNT) TO
                           TOKEN-TYPE-LPAREN
                       WHEN ")"
                           ADD 1 TO LEXER-OUTPUT-TOKEN-TYPE-LIST-COUNT

                           SET LEXER-OUTPUT-TOKEN-TYPE-LIST
                           (LEXER-OUTPUT-TOKEN-TYPE-LIST-COUNT) TO
                           TOKEN-TYPE-RPAREN
                       WHEN "{"
                           ADD 1 TO LEXER-OUTPUT-TOKEN-TYPE-LIST-COUNT

                           SET LEXER-OUTPUT-TOKEN-TYPE-LIST
                           (LEXER-OUTPUT-TOKEN-TYPE-LIST-COUNT) TO
                           TOKEN-TYPE-LSQUIRLY
                       WHEN "}"
                           ADD 1 TO LEXER-OUTPUT-TOKEN-TYPE-LIST-COUNT

                           SET LEXER-OUTPUT-TOKEN-TYPE-LIST
                           (LEXER-OUTPUT-TOKEN-TYPE-LIST-COUNT) TO
                           TOKEN-TYPE-RSQUIRLY
                       WHEN OTHER
                           ADD 1 TO LEXER-OUTPUT-TOKEN-TYPE-LIST-COUNT

                           SET LEXER-OUTPUT-TOKEN-TYPE-LIST
                           (LEXER-OUTPUT-TOKEN-TYPE-LIST-COUNT) TO
                           TOKEN-TYPE-ILLEGAL
                   END-EVALUATE
               END-PERFORM
           END-PERFORM
           ADD 1 TO LEXER-OUTPUT-TOKEN-TYPE-LIST-COUNT
           SET LEXER-OUTPUT-TOKEN-TYPE-LIST
           (LEXER-OUTPUT-TOKEN-TYPE-LIST-COUNT) TO
           TOKEN-TYPE-EOF.

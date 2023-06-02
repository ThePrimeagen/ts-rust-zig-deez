       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEXER-TESTS-PROGRAM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "lexer-data.cpy".
       01 WS-LEXER-TESTS-EXPECTED PIC 99 OCCURS 9 TIMES.
      * okay i wanted nice names but i won't write
      * WS-LEXER-TESTS-COUNTER everywhere
       01 WS-COUNTER PIC 99 VALUE 1.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM LEXER-INIT-PROCEDURE.
      * COBOL is 1 indexed btw
           MOVE "=+,;(}" TO LEXER-INPUT-LINES (1).
      * setting up multiple lines to see if it actually works
           MOVE "){" TO LEXER-INPUT-LINES (2).

           PERFORM LEXER-PARSE-PROCEDURE.

           SET WS-LEXER-TESTS-EXPECTED (1) TO TOKEN-TYPE-ASSIGN.
           SET WS-LEXER-TESTS-EXPECTED (2) TO TOKEN-TYPE-PLUS.
           SET WS-LEXER-TESTS-EXPECTED (3) TO TOKEN-TYPE-COMMA.
           SET WS-LEXER-TESTS-EXPECTED (4) TO TOKEN-TYPE-SEMICOLON.
           SET WS-LEXER-TESTS-EXPECTED (5) TO TOKEN-TYPE-LPAREN.
           SET WS-LEXER-TESTS-EXPECTED (6) TO TOKEN-TYPE-RSQUIRLY.
           SET WS-LEXER-TESTS-EXPECTED (7) TO TOKEN-TYPE-RPAREN.
           SET WS-LEXER-TESTS-EXPECTED (8) TO TOKEN-TYPE-LSQUIRLY.
           SET WS-LEXER-TESTS-EXPECTED (9) TO TOKEN-TYPE-EOF.

           IF LEXER-OUTPUT-TOKEN-TYPE-LIST-COUNT NOT EQUAL TO 9
               DISPLAY "Invalid token count: "
               LEXER-OUTPUT-TOKEN-TYPE-LIST-COUNT ", expected 9."
               STOP RUN RETURNING -1
           END-IF.

           PERFORM VARYING WS-COUNTER FROM 1 BY 1 UNTIL WS-COUNTER > 9
               IF LEXER-OUTPUT-TOKEN-TYPE-LIST (WS-COUNTER) NOT EQUAL TO
                   WS-LEXER-TESTS-EXPECTED (WS-COUNTER)

                   DISPLAY "Invalid token "
                   LEXER-OUTPUT-TOKEN-TYPE-LIST (WS-COUNTER)
                   " at index " WS-COUNTER ", expected "
                   WS-LEXER-TESTS-EXPECTED (WS-COUNTER)

                   STOP RUN RETURNING -2
               END-IF
           END-PERFORM

           DISPLAY "Test passed!"
           STOP RUN.

       COPY "lexer-proc.cpy".

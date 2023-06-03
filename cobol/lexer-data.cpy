       COPY "token-data.cpy".

       01 LEXER-INPUT.
      * yes, that means line can be only 256 chars long and yes,
      * there can only be 1024 lines and yes, this is preallocated
      * (i think; i have no idea how this language works)
           05 LEXER-INPUT-LINES PIC X(256) OCCURS 1024 TIMES.

      * i should probably print this and test stdout instead of this
      * nonsense, but oh well
       01 LEXER-OUTPUT.
      * this is insane, how do you even program without heap
           05 LEXER-OUTPUT-TOKEN-TYPE-LIST PIC 99 OCCURS 65536 TIMES.
           05 LEXER-OUTPUT-TOKEN-TYPE-LIST-COUNT PIC 9(5) VALUE 0.

      * i saw 'WS' prefix somewhere on the internet and i think it's
      * supposed to indicate "local" variables. still need to make names
      * unique tho.
       01 WS-LEXER-LINE-IDX  PIC 9999 VALUE 0.
       01 WS-LEXER-LINE      PIC X(256).
       01 WS-LEXER-CH-IDX    PIC 9999 VALUE 0.
       01 WS-LEXER-CH        PIC X.

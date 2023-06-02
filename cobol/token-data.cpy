       01 TOKEN-TYPE-CONSTS.
           05 TOKEN-TYPE-ILLEGAL    PIC 99 VALUE  0.
           05 TOKEN-TYPE-EOF        PIC 99 VALUE  1.

      * currently multi-character tokens are not parsed
           05 TOKEN-TYPE-IDENT      PIC 99 VALUE  2.
           05 TOKEN-TYPE-INT        PIC 99 VALUE  3.

           05 TOKEN-TYPE-ASSIGN     PIC 99 VALUE  4.
           05 TOKEN-TYPE-PLUS       PIC 99 VALUE  5.
           05 TOKEN-TYPE-COMMA      PIC 99 VALUE  6.
           05 TOKEN-TYPE-SEMICOLON  PIC 99 VALUE  7.
           05 TOKEN-TYPE-LPAREN     PIC 99 VALUE  8.
           05 TOKEN-TYPE-RPAREN     PIC 99 VALUE  9.
           05 TOKEN-TYPE-LSQUIRLY   PIC 99 VALUE 10.
           05 TOKEN-TYPE-RSQUIRLY   PIC 99 VALUE 11.

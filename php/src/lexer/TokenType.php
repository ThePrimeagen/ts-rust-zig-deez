<?php

enum TokenType {
    case Illegal;
    case Eof;
    case Identifier;
    case Integer;
    case Assign;
    case Plus;
    case Minus;
    case Not;
    case Asterisk;
    case Slash;
    case LessThan;
    case GreaterThan;
    case Comma;
    case Semicolon;
    case LeftParen;
    case RightParen;
    case LeftBrace;
    case RightBrace;
    case LeftBracket;
    case RightBracket;
    case Colon;
    case Function;
    case Let;
    case True;
    case False;
    case If;
    case Else;
    case Return;
    case Equals;
    case NotEquals;
    case String;
    case UnterminatedString;
}

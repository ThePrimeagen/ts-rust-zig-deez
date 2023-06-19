<?php

require 'vendor/autoload.php';

enum TokenType {
    case Assign;

    case Asterisk;

    case Colon;

    case Comma;

    case Else;

    case EOF;

    case Equals;

    case False;

    case Function;

    case GreaterThan;

    case Identifier;

    case If;

    case Illegal;

    case Integer;

    case LeftBrace;

    case LeftBracket;

    case LeftParen;

    case LessThan;

    case Let;

    case Minus;

    case Not;

    case NotEquals;

    case Plus;

    case Return;

    case RightBrace;

    case RightBracket;

    case RightParen;

    case Semicolon;

    case Slash;

    case String;

    case True;

    case UnterminatedString;
}

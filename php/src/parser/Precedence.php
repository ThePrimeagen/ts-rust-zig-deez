<?php

enum Precedence: int {
    case Lowest = 1;
    case Equals = 2;        // ==
    case LessGreater = 3;   // > or <
    case Sum = 4;           // +
    case Product = 5;       // *
    case Prefix = 6;        // -X or !X
    case Call = 7;          // myFunction(X)
    case Index = 8;         // array[index]
}


function tokenToPrecedence(TokenType $tokenType): Precedence {
    return match ($tokenType) {
        TokenType::Equals, TokenType::NotEquals => Precedence::Equals,
        TokenType::LessThan, TokenType::GreaterThan => Precedence::LessGreater,
        TokenType::Plus, TokenType::Minus => Precedence::Sum,
        TokenType::Slash, TokenType::Asterisk => Precedence::Product,
        TokenType::LeftParen => Precedence::Call,
        TokenType::LeftBracket => Precedence::Index,
        default => Precedence::Lowest,
    };
}

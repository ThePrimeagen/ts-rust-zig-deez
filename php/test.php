<?php

require_once "Tokenizer.php";

$tokenizer = new Tokenizer("
let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};
let result = add(five, ten);");


$expectedTokens = [
    new Token(TokenType::Let),
    new Token(TokenType::Ident, "five"),
    new Token(TokenType::Equal),
    new Token(TokenType::Int, "5"),
    new Token(TokenType::Semicolon),
    new Token(TokenType::Let),
    new Token(TokenType::Ident, "ten"),
    new Token(TokenType::Equal),
    new Token(TokenType::Int, "10"),
    new Token(TokenType::Semicolon),
    new Token(TokenType::Let),
    new Token(TokenType::Ident, "add"),
    new Token(TokenType::Equal),
    new Token(TokenType::Function),
    new Token(TokenType::LeftParen),
    new Token(TokenType::Ident, "x"),
    new Token(TokenType::Comma),
    new Token(TokenType::Ident, "y"),
    new Token(TokenType::RightParen),
    new Token(TokenType::LeftBrace),
    new Token(TokenType::Ident, "x"),
    new Token(TokenType::Plus),
    new Token(TokenType::Ident, "y"),
    new Token(TokenType::Semicolon),
    new Token(TokenType::RightBrace),
    new Token(TokenType::Semicolon),
    new Token(TokenType::Let), // {
    new Token(TokenType::Ident, "result"),
    new Token(TokenType::Equal),
    new Token(TokenType::Ident, "add"),
    new Token(TokenType::LeftParen),
    new Token(TokenType::Ident, "five"),
    new Token(TokenType::Comma),
    new Token(TokenType::Ident, "ten"),
    new Token(TokenType::RightParen),
    new Token(TokenType::Semicolon),
    new Token(TokenType::Eof),
];

$i = 0;

do {
    $token = $tokenizer->getNextToken();
    $expectedToken = $expectedTokens[$i++];
    echo $token->type->name . " " . $token->literal . "\n";

    if ($token->type !== $expectedToken->type || $token->literal !== $expectedToken->literal) {
        echo PHP_EOL, PHP_EOL;
        echo "Token $i failed", PHP_EOL;
        echo "Got {$token->type->name} $token->literal", PHP_EOL;
        echo "Expected {$expectedToken->type->name} $expectedToken->literal", PHP_EOL;
        break;
    }
} while ($token->type !== TokenType::Eof);

if ($i === count($expectedTokens)) {
    echo "All tests passed", PHP_EOL;
}

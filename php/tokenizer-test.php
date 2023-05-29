<?php

require_once "Tokenizer.php";


//$tokenizer = new Tokenizer("
//let five = 5;
//let ten = 10;
//let add = fn(x, y) {
//    x + y;
//};
//let result = add(five, ten);");
//
//
//$expectedTokens = [
//    new Token(TokenType::Let),
//    new Token(TokenType::Identifier, "five"),
//    new Token(TokenType::Assign),
//    new Token(TokenType::Integer, "5"),
//    new Token(TokenType::Semicolon),
//    new Token(TokenType::Let),
//    new Token(TokenType::Identifier, "ten"),
//    new Token(TokenType::Assign),
//    new Token(TokenType::Integer, "10"),
//    new Token(TokenType::Semicolon),
//    new Token(TokenType::Let),
//    new Token(TokenType::Identifier, "add"),
//    new Token(TokenType::Assign),
//    new Token(TokenType::Function),
//    new Token(TokenType::LeftParen),
//    new Token(TokenType::Identifier, "x"),
//    new Token(TokenType::Comma),
//    new Token(TokenType::Identifier, "y"),
//    new Token(TokenType::RightParen),
//    new Token(TokenType::LeftBrace),
//    new Token(TokenType::Identifier, "x"),
//    new Token(TokenType::Plus),
//    new Token(TokenType::Identifier, "y"),
//    new Token(TokenType::Semicolon),
//    new Token(TokenType::RightBrace),
//    new Token(TokenType::Semicolon),
//    new Token(TokenType::Let), // {
//    new Token(TokenType::Identifier, "result"),
//    new Token(TokenType::Assign),
//    new Token(TokenType::Identifier, "add"),
//    new Token(TokenType::LeftParen),
//    new Token(TokenType::Identifier, "five"),
//    new Token(TokenType::Comma),
//    new Token(TokenType::Identifier, "ten"),
//    new Token(TokenType::RightParen),
//    new Token(TokenType::Semicolon),
//    new Token(TokenType::Eof),
//];
//
//$i = 0;
//
//do {
//    $token = $tokenizer->getNextToken();
//    $expectedToken = $expectedTokens[$i++];
//    echo $token->type->name . " " . $token->literal . "\n";
//
//    if ($token->type !== $expectedToken->type || $token->literal !== $expectedToken->literal) {
//        echo PHP_EOL, PHP_EOL;
//        echo "Token $i failed", PHP_EOL;
//        echo "Got {$token->type->name} $token->literal", PHP_EOL;
//        echo "Expected {$expectedToken->type->name} $expectedToken->literal", PHP_EOL;
//        break;
//    }
//} while ($token->type !== TokenType::Eof);
//
//if ($i === count($expectedTokens)) {
//    echo "All tests passed", PHP_EOL;
//}


$tokenizer = new Tokenizer('let string = "caca""sdfsdfdsf\\"');
do {
    $token = $tokenizer->getNextToken();
    echo $token->type->name . " " . $token->literal . "\n";
} while ($token->type !== TokenType::Eof);

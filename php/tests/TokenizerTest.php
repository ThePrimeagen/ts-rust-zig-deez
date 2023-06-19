<?php

declare(strict_types=1);

use PHPUnit\Framework\TestCase;

final class TokenizerTest extends TestCase
{
    public function testTokenizer(): void
    {
        $expected = [
            new Token(TokenType::Let),
            new Token(TokenType::Identifier, "five"),
            new Token(TokenType::Assign),
            new Token(TokenType::Integer, "5"),
            new Token(TokenType::Semicolon),
            new Token(TokenType::Let),
            new Token(TokenType::Identifier, "ten"),
            new Token(TokenType::Assign),
            new Token(TokenType::Integer, "10"),
            new Token(TokenType::Semicolon),
            new Token(TokenType::Let),
            new Token(TokenType::Identifier, "add"),
            new Token(TokenType::Assign),
            new Token(TokenType::Function),
            new Token(TokenType::LeftParen),
            new Token(TokenType::Identifier, "x"),
            new Token(TokenType::Comma),
            new Token(TokenType::Identifier, "y"),
            new Token(TokenType::RightParen),
            new Token(TokenType::LeftBrace),
            new Token(TokenType::Identifier, "x"),
            new Token(TokenType::Plus),
            new Token(TokenType::Identifier, "y"),
            new Token(TokenType::Semicolon),
            new Token(TokenType::RightBrace),
            new Token(TokenType::Semicolon),
            new Token(TokenType::Let), // {
            new Token(TokenType::Identifier, "result"),
            new Token(TokenType::Assign),
            new Token(TokenType::Identifier, "add"),
            new Token(TokenType::LeftParen),
            new Token(TokenType::Identifier, "five"),
            new Token(TokenType::Comma),
            new Token(TokenType::Identifier, "ten"),
            new Token(TokenType::RightParen),
            new Token(TokenType::Semicolon),
            new Token(TokenType::Eof),
        ];
        $tokenizer = new Tokenizer("
        let five = 5;
        let ten = 10;
        let add = fn(x, y) {
            x + y;
        };
        let result = add(five, ten);");

        $i = 0;
        do {
            $token = $tokenizer->getNextToken();
            $expectedToken = $expected[$i++];
            $this->assertEquals($expectedToken, $token);
        } while ($token->type !== TokenType::Eof);
    }
}

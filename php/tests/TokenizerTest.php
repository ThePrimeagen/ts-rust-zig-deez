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
            new Token(TokenType::Let),
            new Token(TokenType::Identifier, "result"),
            new Token(TokenType::Assign),
            new Token(TokenType::Identifier, "add"),
            new Token(TokenType::LeftParen),
            new Token(TokenType::Identifier, "five"),
            new Token(TokenType::Comma),
            new Token(TokenType::Identifier, "ten"),
            new Token(TokenType::RightParen),
            new Token(TokenType::Semicolon),
            new Token(TokenType::Not),
            new Token(TokenType::Minus),
            new Token(TokenType::Slash),
            new Token(TokenType::Asterisk),
            new Token(TokenType::Integer, "5"),
            new Token(TokenType::Semicolon),
            new Token(TokenType::Integer, "5"),
            new Token(TokenType::LessThan),
            new Token(TokenType::Integer, "10"),
            new Token(TokenType::GreaterThan),
            new Token(TokenType::Integer, "5"),
            new Token(TokenType::Semicolon),
            new Token(TokenType::If),
            new Token(TokenType::LeftParen),
            new Token(TokenType::Integer, "5"),
            new Token(TokenType::LessThan),
            new Token(TokenType::Integer, "10"),
            new Token(TokenType::RightParen),
            new Token(TokenType::LeftBrace),
            new Token(TokenType::Return),
            new Token(TokenType::True),
            new Token(TokenType::Semicolon),
            new Token(TokenType::RightBrace),
            new Token(TokenType::Else),
            new Token(TokenType::LeftBrace),
            new Token(TokenType::Return),
            new Token(TokenType::False),
            new Token(TokenType::Semicolon),
            new Token(TokenType::RightBrace),
            new Token(TokenType::Integer, "10"),
            new Token(TokenType::Equals),
            new Token(TokenType::Integer, "10"),
            new Token(TokenType::Semicolon),
            new Token(TokenType::Integer, "10"),
            new Token(TokenType::NotEquals),
            new Token(TokenType::Integer, "9"),
            new Token(TokenType::Semicolon),
            new Token(TokenType::String, "foobar"),
            new Token(TokenType::String, "foo bar"),
            new Token(TokenType::LeftBracket),
            new Token(TokenType::Integer, "1"),
            new Token(TokenType::Comma),
            new Token(TokenType::Integer, "2"),
            new Token(TokenType::RightBracket),
            new Token(TokenType::Semicolon),
            new Token(TokenType::LeftBrace),
            new Token(TokenType::String, "foo"),
            new Token(TokenType::Colon),
            new Token(TokenType::String, "bar"),
            new Token(TokenType::RightBrace),
            new Token(TokenType::EOF),
        ];
        $tokenizer = new Tokenizer("
            let five = 5;
            let ten = 10;

            let add = fn(x, y) {
                x + y;
            };

            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
                return true;
            } else {
                return false;
            }
            10 == 10;
            10 != 9;
            \"foobar\"
            \"foo bar\"
            [1, 2];
            {\"foo\": \"bar\"}

        ");

        $i = 0;
        do {
            $token = $tokenizer->getNextToken();
            $expectedToken = $expected[$i++];
            $this->assertEquals($expectedToken, $token);
        } while ($token->type !== TokenType::EOF);
    }
}

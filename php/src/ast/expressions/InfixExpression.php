<?php

require 'vendor/autoload.php';

readonly class InfixExpression implements Expression
{

    public function __construct(
        public Token $token,
        public Expression $left,
        public Expression $right
    ) {
    }

    public function expressionNode(): void
    {
    }

    public function tokenLiteral(): string
    {
        return $this->token->literal;
    }

    public function __toString(): string
    {
        $operator = match ($this->token->type) {
            TokenType::Equals => "==",
            TokenType::NotEquals => "!=",
            TokenType::LessThan => "<",
            TokenType::GreaterThan => ">",
            TokenType::Plus => "+",
            TokenType::Minus => "-",
            TokenType::Slash => "/",
            TokenType::Asterisk => "*",
            default => "<unknown>",
        };

        return "(" . $this->left . " " . $operator . " " . $this->right . ")";
    }
}
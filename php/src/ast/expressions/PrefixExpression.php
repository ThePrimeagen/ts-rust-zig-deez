<?php

require 'vendor/autoload.php';

readonly class PrefixExpression implements Expression
{
    public function __construct(
        public Token $token,
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
            TokenType::Not => "!",
            TokenType::Minus => "-",
            default => "<unknown>",
        };

        return "(" . $operator . $this->right . ")";
    }
}
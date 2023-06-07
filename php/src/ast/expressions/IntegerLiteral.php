<?php

require 'vendor/autoload.php';

readonly class IntegerLiteral implements Expression
{
    public function __construct(
        public Token $token,
        public int $value
    ) {
    }

    public function tokenLiteral(): string
    {
        return $this->token->literal;
    }

    public function expressionNode(): void
    {
    }

    public function __toString(): string
    {
        return (string) $this->value;
    }
}
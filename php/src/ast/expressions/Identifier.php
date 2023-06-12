<?php

require 'vendor/autoload.php';

readonly class Identifier implements Expression
{
    public function __construct(
        public Token $token,
        public string $value
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
        return $this->value;
    }
}
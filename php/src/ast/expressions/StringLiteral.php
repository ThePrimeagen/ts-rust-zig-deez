<?php

require 'vendor/autoload.php';

readonly class StringLiteral implements Expression
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
        return json_encode($this->value);
    }
}
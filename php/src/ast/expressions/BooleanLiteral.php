<?php

require 'vendor/autoload.php';

readonly class BooleanLiteral implements Expression
{
    public function __construct(
        public Token $token,
        public bool $value
    ) {
    }

    public function tokenLiteral(): string
    {
        return $this->value ? "true" : "false";
    }

    public function expressionNode(): void
    {
    }

    public function __toString(): string
    {
        return $this->value ? "true" : "false";
    }
}
<?php

require 'vendor/autoload.php';

readonly class LetStatement implements Statement
{

    public function __construct(
        public Token $token,
        public Identifier $identifier,
        public Expression $value
    ) {
    }

    public function statementNode(): void
    {
    }

    public function tokenLiteral(): string
    {
        return $this->token->literal;
    }

    public function __toString(): string
    {
        return "let " . $this->identifier . " = " . $this->value . ";";
    }
}
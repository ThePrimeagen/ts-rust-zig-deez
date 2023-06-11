<?php

require 'vendor/autoload.php';

readonly class ReturnStatement implements Statement
{
    public function __construct(
        public Token $token,
        public Expression $returnValue
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
        return "return " . $this->returnValue . ";";
    }
}
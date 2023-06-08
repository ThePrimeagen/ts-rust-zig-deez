<?php

require 'vendor/autoload.php';

readonly class ExpressionStatement implements Statement
{

    public function __construct(
        public Token $token,
        public Expression $expression
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
        return $this->expression . ";";
    }
}
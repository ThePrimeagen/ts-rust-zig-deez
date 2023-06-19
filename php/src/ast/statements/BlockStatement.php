<?php

require 'vendor/autoload.php';

readonly class BlockStatement implements Statement
{

    /**
     * @param Token $token
     * @param Statement[] $statements
     */
    public function __construct(
        public Token $token,
        public array $statements
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
        return "{" . implode("", $this->statements) . "}";
    }
}
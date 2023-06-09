<?php

require 'vendor/autoload.php';

readonly class FunctionLiteral implements Expression
{
    /**
     * @param Token $token
     * @param Identifier[] $parameters
     * @param BlockStatement $body
     */
    public function __construct(
        public Token $token,
        public array $parameters,
        public BlockStatement $body
    ) {
    }

    public function tokenLiteral(): string
    {
        return "fn";
    }

    public function __toString(): string
    {
        return "fn(" . implode(", ", $this->parameters) . ") " . $this->body;
    }

    public function expressionNode(): void
    {
    }
}
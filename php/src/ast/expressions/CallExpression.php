<?php

require 'vendor/autoload.php';

readonly class CallExpression implements Expression
{

    /**
     * @param Token $token
     * @param Expression $function
     * @param Expression[] $arguments
     */
    public function __construct(
        public Token $token,
        public Expression $function,
        public array $arguments
    ) {
    }

    public function expressionNode(): void
    {
    }

    public function tokenLiteral(): string
    {
        return "(";
    }

    public function __toString(): string
    {
        return $this->function . "(" . implode(", ", $this->arguments) . ")";
    }
}

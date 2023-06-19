<?php

require 'vendor/autoload.php';

readonly class FunctionLiteral implements Expression {
    /**
     * @param Identifier[] $parameters
     */
    public function __construct(
        public Token $token,
        public array $parameters,
        public BlockStatement $body
    ) {
    }

    public function __toString(): string {
        return 'fn('.implode(', ', $this->parameters).') '.$this->body;
    }

    public function tokenLiteral(): string {
        return 'fn';
    }

    public function expressionNode(): void {
    }
}

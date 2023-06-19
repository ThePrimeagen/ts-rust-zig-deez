<?php

readonly class ArrayLiteral implements Expression {
    /**
     * @param Token $token
     * @param Expression[] $elements
     */
    public function __construct(
        public Token $token,
        public array $elements,
    ) {
    }

    public function expressionNode(): void {
    }

    public function tokenLiteral(): string {
        return "[";
    }

    public function __toString(): string {
        return "[" . implode(", ", $this->elements) . "]";
    }
}

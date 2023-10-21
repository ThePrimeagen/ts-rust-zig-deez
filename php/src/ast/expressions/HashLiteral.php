<?php

readonly class HashLiteral implements Expression {


    /**
     * @param Token $token
     * @param SplObjectStorage $pairs
     */
    public function __construct(
        public Token $token,
        public SplObjectStorage $pairs,
    ) {
    }

    public function expressionNode(): void {
    }

    public function tokenLiteral(): string {
        return "{";
    }

    public function __toString(): string {
        $out = [];
        /** @var Expression $key */
        /** @var Expression $value */
        foreach ($this->pairs as $key) {
            $value = $this->pairs[$key];
            $out[] = "{$key}: {$value}";
        }
        return "{" . implode(", ", $out) . "}";
    }
}

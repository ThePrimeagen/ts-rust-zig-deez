<?php

readonly class IndexExpression implements Expression {

    public function __construct(
        public Token $token,
        public Expression $left,
        public Expression $index,
    ) {
    }

    public function expressionNode(): void {
    }

    public function tokenLiteral(): string {
        return "[";
    }

    public function __toString(): string {
        return "({$this->left}[{$this->index}])";
    }
}

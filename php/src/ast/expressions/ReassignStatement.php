<?php

readonly class ReassignStatement implements Statement {

    public function __construct(
        public Identifier $identifier,
        public Expression $value
    ) {
    }

    public function statementNode(): void {
    }

    public function tokenLiteral(): string {
        return $this->identifier->value;
    }

    public function __toString(): string {
        return $this->identifier . " = " . $this->value . ";";
    }
}

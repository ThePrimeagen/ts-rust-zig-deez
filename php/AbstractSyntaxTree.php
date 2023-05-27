<?php

require_once "Tokenizer.php";

interface Node extends Stringable {
    public function tokenLiteral(): string;
    public function __toString(): string;
}

interface Statement extends Node {
    public function statementNode(): void;
}

interface Expression extends Node {
    public function expressionNode(): void;
}

readonly class Identifier implements Expression {
    public Token $token;
    public string $value;

    public function __construct(Token $token, string $value) {
        $this->token = $token;
        $this->value = $value;
    }

    public function tokenLiteral(): string {
        return $this->token->literal;
    }

    public function expressionNode(): void {}

    public function __toString(): string {
        return $this->value;
    }
}

readonly class IntegerLiteral implements Expression {
    public Token $token;
    public int $value;

    public function __construct(Token $token, int $value) {
        $this->token = $token;
        $this->value = $value;
    }

    public function tokenLiteral(): string {
        return $this->token->literal;
    }

    public function expressionNode(): void {}

    public function __toString(): string {
        return (string) $this->value;
    }
}

readonly class BooleanLiteral implements Expression {
    public Token $token;
    public bool $value;

    public function __construct(Token $token, bool $value) {
        $this->token = $token;
        $this->value = $value;
    }

    public function tokenLiteral(): string {
        return $this->value ? "true" : "false";
    }

    public function expressionNode(): void {}

    public function __toString(): string {
        return $this->value ? "true" : "false";
    }
}


class Program implements Node {
    /** @var Statement[] */
    private array $statements = [];

    public function tokenLiteral(): string {
        if (count($this->statements) > 0) {
            return $this->statements[0]->tokenLiteral();
        } else {
            return "";
        }
    }

    public function appendStatement(Statement $statement): void {
        $this->statements[] = $statement;
    }

    public function __toString(): string {
        return implode("", $this->statements);
    }
}

readonly class LetStatement implements Statement {

    public Token $token;
    public Identifier $identifier;
    public Expression $value;

    public function __construct(Token $token, Identifier $identifier, Expression $value) {
        $this->token = $token;
        $this->identifier = $identifier;
        $this->value = $value;
    }

    public function statementNode(): void {

    }

    public function tokenLiteral(): string {
        return $this->token->literal;
    }

    public function __toString(): string {
        return "let " . $this->identifier . " = " . $this->value . ";";
    }
}

readonly class ReturnStatement implements Statement {
    public Token $token;
    public Expression $returnValue;

    public function __construct(Token $token, Expression $returnValue) {
        $this->token = $token;
        $this->returnValue = $returnValue;
    }

    public function statementNode(): void {

    }

    public function tokenLiteral(): string {
        return $this->token->literal;
    }

    public function __toString(): string {
        return "return " . $this->returnValue . ";";
    }
}

readonly class ExpressionStatement implements Statement {
    public Token $token;
    public Expression $expression;

    public function __construct(Token $token, Expression $expression) {
        $this->token = $token;
        $this->expression = $expression;
    }

    public function statementNode(): void {

    }

    public function tokenLiteral(): string {
        return $this->token->literal;
    }

    public function __toString(): string {
        return $this->expression;
    }
}

readonly class PrefixExpression implements Expression {
    public Token $token;
    public Expression $right;

    public function __construct(Token $token, Expression $right) {
        $this->token = $token;
        $this->right = $right;
    }

    public function expressionNode(): void {

    }

    public function tokenLiteral(): string {
        return $this->token->literal;
    }

    public function __toString(): string {
        $operator = match ($this->token->type) {
            TokenType::Not => "!",
            TokenType::Minus => "-",
            default => "<unknown>",
        };

        return "(" . $operator . $this->right . ")";
    }
}

readonly class InfixExpression implements Expression {
    public Token $token;
    public Expression $left;
    public Expression $right;

    public function __construct(Token $token, Expression $left, Expression $right) {
        $this->token = $token;
        $this->left = $left;
        $this->right = $right;
    }

    public function expressionNode(): void {

    }

    public function tokenLiteral(): string {
        return $this->token->literal;
    }

    public function __toString(): string {
        $operator = match ($this->token->type) {
            TokenType::Equals => "==",
            TokenType::NotEquals => "!=",
            TokenType::LessThan => "<",
            TokenType::GreaterThan => ">",
            TokenType::Plus => "+",
            TokenType::Minus => "-",
            TokenType::Slash => "/",
            TokenType::Asterisk => "*",
            default => "<unknown>",
        };

        return "(" . $this->left . " " . $operator . " " . $this->right . ")";
    }
}

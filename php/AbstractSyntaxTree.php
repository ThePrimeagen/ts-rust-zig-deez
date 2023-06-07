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
    public function __construct(public Token $token,
                                public string $value) {
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
    public function __construct(public Token $token,
                                public int $value) {
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
    public function __construct(public Token $token,
                                public bool $value) {
    }

    public function tokenLiteral(): string {
        return $this->value ? "true" : "false";
    }

    public function expressionNode(): void {}

    public function __toString(): string {
        return $this->value ? "true" : "false";
    }
}

readonly class FunctionLiteral implements Expression {
    /**
     * @param Token $token
     * @param Identifier[] $parameters
     * @param BlockStatement $body
     */
    public function __construct(public Token $token,
                                public array $parameters,
                                public BlockStatement $body) {
    }

    public function tokenLiteral(): string {
        return "fn";
    }

    public function __toString(): string {
        return "fn(" . implode(", ", $this->parameters) . ") " . $this->body;
    }

    public function expressionNode(): void {
    }
}

readonly class StringLiteral implements Expression {
    public function __construct(public Token $token,
                                public string $value) {
    }

    public function tokenLiteral(): string {
        return $this->token->literal;
    }

    public function expressionNode(): void {}

    public function __toString(): string {
        return json_encode($this->value);
    }
}


class Program implements Node {
    /** @var Statement[] */
    public array $statements;

    public function __construct() {
        $this->statements = [];
    }


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

    public function __construct(public Token $token,
                                public Identifier $identifier,
                                public Expression $value) {
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
    public function __construct(public Token $token,
                                public Expression $returnValue) {
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

    public function __construct(public Token $token,
                                public Expression $expression) {
    }

    public function statementNode(): void {

    }

    public function tokenLiteral(): string {
        return $this->token->literal;
    }

    public function __toString(): string {
        return $this->expression . ";";
    }
}

readonly class PrefixExpression implements Expression {
    public function __construct(public Token $token,
                                public Expression $right) {
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

    public function __construct(public Token $token,
                                public Expression $left,
                                public Expression $right) {
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

readonly class IfExpression implements Expression {
    public function __construct(public Token $token,
                                public Expression $condition,
                                public BlockStatement $consequence,
                                public ?BlockStatement $alternative) {
    }

    public function expressionNode(): void {

    }

    public function tokenLiteral(): string {
        return "if";
    }

    public function __toString(): string {
        $out = "if $this->condition $this->consequence";

        if ($this->alternative !== null) {
            $out .= " else $this->alternative";
        }

        return $out;
    }
}

readonly class BlockStatement implements Statement {

    /**
     * @param Token $token
     * @param Statement[] $statements
     */
    public function __construct(public Token $token,
                                public array $statements) {
    }

    public function statementNode(): void {

    }

    public function tokenLiteral(): string {
        return $this->token->literal;
    }

    public function __toString(): string {
        return "{" . implode("", $this->statements) . "}";
    }
}

readonly class CallExpression implements Expression {

    /**
     * @param Token $token
     * @param Expression $function
     * @param Expression[] $arguments
     */
    public function __construct(public Token $token,
                                public Expression $function,
                                public array $arguments) {
    }

    public function expressionNode(): void {

    }

    public function tokenLiteral(): string {
        return "(";
    }

    public function __toString(): string {
        return $this->function . "(" . implode(", ", $this->arguments) . ")";
    }
}

<?php

require_once "Tokenizer.php";
require_once "AbstractSyntaxTree.php";

enum Precedence: int {
    case Lowest = 1;
    case Equals = 2;        // ==
    case LessGreater = 3;   // > or <
    case Sum = 4;           // +
    case Product = 5;       // *
    case Prefix = 6;        // -X or !X
    case Call = 7;          // myFunction(X)
}

function tokenToPrecedence(TokenType $tokenType): Precedence {
    return match ($tokenType) {
        TokenType::Equals, TokenType::NotEquals => Precedence::Equals,
        TokenType::LessThan, TokenType::GreaterThan => Precedence::LessGreater,
        TokenType::Plus, TokenType::Minus => Precedence::Sum,
        TokenType::Slash, TokenType::Asterisk => Precedence::Product,
        TokenType::LeftParen => Precedence::Call,
        default => Precedence::Lowest,
    };
}

class Parser {
    private readonly Tokenizer $tokenizer;

    private Token $currentToken;
    private Token $peekToken;

    /** @var string[] */
    private array $errors = [];

    /** @var Closure<Expression>[]  */
    private array $prefixParseFns = [];
    /** @var Closure[]  */
    private array $infixParseFns = [];

    /**
     * @param Tokenizer $tokenizer
     */
    public function __construct(Tokenizer $tokenizer) {
        $this->tokenizer = $tokenizer;

        $this->currentToken = $this->tokenizer->getNextToken();
        $this->peekToken = $this->tokenizer->getNextToken();

        $this->registerPrefix(TokenType::Identifier, fn() => $this->parseIdentifier());
        $this->registerPrefix(TokenType::Integer, fn() => $this->parseIntegerLiteral());
        $this->registerPrefix(TokenType::Not, fn() => $this->parsePrefixExpression());
        $this->registerPrefix(TokenType::Minus, fn() => $this->parsePrefixExpression());
        $this->registerPrefix(TokenType::True, fn() => $this->parseBoolean());
        $this->registerPrefix(TokenType::False, fn() => $this->parseBoolean());
        $this->registerPrefix(TokenType::LeftParen, fn() => $this->parseGroupedExpression());
        $this->registerPrefix(TokenType::If, fn() => $this->parseIfExpression());
        $this->registerPrefix(TokenType::Function, fn() => $this->parseFunctionLiteral());
        $this->registerPrefix(TokenType::String, fn() => $this->parseStringLiteral());
        $this->registerPrefix(TokenType::UnterminatedString, fn() => $this->handleUnterminatedString());

        $this->registerInfix(TokenType::Plus, fn(Expression $left) => $this->parseInfixExpression($left));
        $this->registerInfix(TokenType::Minus, fn(Expression $left) => $this->parseInfixExpression($left));
        $this->registerInfix(TokenType::Slash, fn(Expression $left) => $this->parseInfixExpression($left));
        $this->registerInfix(TokenType::Asterisk, fn(Expression $left) => $this->parseInfixExpression($left));
        $this->registerInfix(TokenType::Equals, fn(Expression $left) => $this->parseInfixExpression($left));
        $this->registerInfix(TokenType::NotEquals, fn(Expression $left) => $this->parseInfixExpression($left));
        $this->registerInfix(TokenType::LessThan, fn(Expression $left) => $this->parseInfixExpression($left));
        $this->registerInfix(TokenType::GreaterThan, fn(Expression $left) => $this->parseInfixExpression($left));
        $this->registerInfix(TokenType::LeftParen, fn(Expression $left) => $this->parseCallExpression($left));
    }

    /**
     * @return string[]
     */
    public function getErrors(): array {
        return $this->errors;
    }

    private function nextToken(): void {
        $this->currentToken = $this->peekToken;
        $this->peekToken = $this->tokenizer->getNextToken();
    }

    private function peekPrecedence(): Precedence {
        return tokenToPrecedence($this->peekToken->type);
    }

    private function currentPrecedence(): Precedence {
        return tokenToPrecedence($this->currentToken->type);
    }

    public function parseProgram(): ?Program {
        $program = new Program();
        try {
            while ($this->currentToken->type !== TokenType::Eof) {
                $statement = $this->parseStatement();

                if ($statement !== null) {
                    $program->appendStatement($statement);
                }

                $this->nextToken();
            }
        } catch (Error $e) {
        }

        return $program;
    }

    private function parseStatement(): ?Statement {
        return match($this->currentToken->type) {
            TokenType::Let => $this->parseLetStatement(),
            TokenType::Return => $this->parseReturnStatement(),
            default => $this->parseExpressionStatement(),
        };
    }

    private function expectPeek(TokenType $tokenType): bool {
        if ($this->peekToken->type === $tokenType) {
            $this->nextToken();
            return true;
        }

        $this->peekError($tokenType);
        return false;
    }

    private function parseLetStatement(): ?LetStatement {
        $letToken = $this->currentToken;

        if (!$this->expectPeek(TokenType::Identifier)) {
            return null;
        }

        $identifier = new Identifier($this->currentToken, $this->currentToken->literal);

        if (!$this->expectPeek(TokenType::Assign)) {
            return null;
        }

        $this->nextToken();

        $letExpression = $this->parseExpression(Precedence::Lowest);

        $this->expectPeek(TokenType::Semicolon);

        return new LetStatement($letToken, $identifier, $letExpression);
    }

    private function parseReturnStatement(): ?ReturnStatement {
        $returnToken = $this->currentToken;
        $this->nextToken();

        $returnExpression = $this->parseExpression(Precedence::Lowest);

        $this->expectPeek(TokenType::Semicolon);

        return new ReturnStatement($returnToken, $returnExpression);
    }

    private function parseExpressionStatement(): ?ExpressionStatement {
        $expression = $this->parseExpression(Precedence::Lowest);

        if ($this->peekToken->type === TokenType::Semicolon) {
            $this->nextToken();
        }

        return new ExpressionStatement($this->currentToken, $expression);
    }

    private function parseExpression(Precedence $precedence): ?Expression {
        $prefix = $this->prefixParseFns[$this->currentToken->type->name] ?? null;
        if ($prefix === null) {
            $this->noPrefixParseFnError($this->currentToken->type);
            return null;
        }

        /** @var Expression $leftExpression */
        $leftExpression = $prefix();

        while ($this->peekToken->type !== TokenType::Semicolon && $precedence->value < $this->peekPrecedence()->value) {
            $infix = $this->infixParseFns[$this->peekToken->type->name] ?? null;
            if ($infix === null) {
                return $leftExpression;
            }

            $this->nextToken();

            /** @var Expression $leftExpression */
            $leftExpression = $infix($leftExpression);
        }
        return $leftExpression;
    }

    private function peekError(TokenType $tokenType): void {
        $expected = $tokenType->name;
        $actual = $this->peekToken->type->name;

        $this->errors[] = "Expected next token to be " . $expected . ", got " . $actual . " instead";
    }

    private function registerPrefix(TokenType $tokenType, Closure $param): void {
        $this->prefixParseFns[$tokenType->name] = $param;
    }

    private function registerInfix(TokenType $tokenType, Closure $param): void {
        $this->infixParseFns[$tokenType->name] = $param;
    }

    private function parseIdentifier(): Identifier {
        return new Identifier($this->currentToken, $this->currentToken->literal);
    }

    private function parseIntegerLiteral(): ?IntegerLiteral {
        $int = (int) $this->currentToken->literal;

        if ($int === PHP_INT_MAX && $this->currentToken->literal !== (string) PHP_INT_MAX) {
            $this->errors[] = "Could not parse " . $this->currentToken->literal . " as integer (too big).";
            return null;
        }

        return new IntegerLiteral($this->currentToken, $int);
    }

    private function parseFunctionLiteral(): ?FunctionLiteral {
        if (!$this->expectPeek(TokenType::LeftParen)) {
            return null;
        }

        $parameters = $this->parseFunctionParameters();

        if (!$this->expectPeek(TokenType::LeftBrace)) {
            return null;
        }

        $body = $this->parseBlockStatement();

        return new FunctionLiteral($this->currentToken, $parameters, $body);
    }

    private function parseStringLiteral(): StringLiteral {
        return new StringLiteral($this->currentToken, $this->currentToken->literal);
    }

    private function handleUnterminatedString(): null {
        $this->errors[] = "Unterminated string literal";
        return null;
    }

    /**
     * @return Identifier[]
     */
    private function parseFunctionParameters(): array {
        /** @var Identifier[] $identifiers */
        $identifiers = [];

        if ($this->peekToken->type === TokenType::RightParen) {
            $this->nextToken();
            return $identifiers;
        }

        $this->nextToken();

        $identifiers[] = new Identifier($this->currentToken, $this->currentToken->literal);


        while ($this->peekToken->type === TokenType::Comma) {
            $this->nextToken();
            $this->nextToken();

            $identifiers[] = new Identifier($this->currentToken, $this->currentToken->literal);
        }

        if (!$this->expectPeek(TokenType::RightParen)) {
            return [];
        }

        return $identifiers;
    }

    private function parseBoolean(): BooleanLiteral {
        return new BooleanLiteral($this->currentToken, $this->currentToken->type === TokenType::True);
    }

    private function parseIfExpression(): ?IfExpression {
        $ifToken = $this->currentToken;

        if (!$this->expectPeek(TokenType::LeftParen)) {
            return null;
        }

        $this->nextToken();

        $condition = $this->parseExpression(Precedence::Lowest);

        if (!$this->expectPeek(TokenType::RightParen)) {
            return null;
        }

        if (!$this->expectPeek(TokenType::LeftBrace)) {
            return null;
        }

        $consequence = $this->parseBlockStatement();

        if ($this->peekToken->type === TokenType::Else) {
            $this->nextToken();

            if (!$this->expectPeek(TokenType::LeftBrace)) {
                return null;
            }

            $alternative = $this->parseBlockStatement();
        } else {
            $alternative = null;
        }

        return new IfExpression($ifToken, $condition, $consequence, $alternative);
    }

    private function noPrefixParseFnError(TokenType $tokenType): void {
        $this->errors[] = "No prefix parse function for " . $tokenType->name . " found";
    }

    private function parsePrefixExpression(): ?PrefixExpression {
        $token = $this->currentToken;
        $this->nextToken();
        $right = $this->parseExpression(Precedence::Prefix);
        return new PrefixExpression($token, $right);
    }

    private function parseInfixExpression(Expression $left): ?InfixExpression {
        $token = $this->currentToken;
        $precedence = $this->currentPrecedence();
        $this->nextToken();
        $right = $this->parseExpression($precedence);
        return new InfixExpression($token, $left, $right);
    }

    private function parseGroupedExpression(): ?Expression {
        $this->nextToken();
        $exp = $this->parseExpression(Precedence::Lowest);

        if (!$this->expectPeek(TokenType::RightParen)) {
            return null;
        }

        return $exp;
    }

    private function parseCallExpression(Expression $left): ?CallExpression {
        $token = $this->currentToken;
        $arguments = $this->parseCallArguments();
        return new CallExpression($token, $left, $arguments);
    }

    private function parseBlockStatement(): BlockStatement {
        /** @var Statement[] $statements */
        $statements = [];

        $this->nextToken();
        while ($this->currentToken->type !== TokenType::RightBrace && $this->currentToken->type !== TokenType::Eof) {
            $statement = $this->parseStatement();
            if ($statement !== null) {
                $statements[] = $statement;
            }
            $this->nextToken();
        }

        return new BlockStatement($this->currentToken, $statements);
    }

    /**
     * @return Expression[]
     */
    private function parseCallArguments(): array {
        /** @var Expression[] $arguments */
        $arguments = [];

        if ($this->peekToken->type === TokenType::RightParen) {
            $this->nextToken();
            return $arguments;
        }

        $this->nextToken();
        $arguments[] = $this->parseExpression(Precedence::Lowest);

        while ($this->peekToken->type === TokenType::Comma) {
            $this->nextToken();
            $this->nextToken();
            $arguments[] = $this->parseExpression(Precedence::Lowest);
        }

        if (!$this->expectPeek(TokenType::RightParen)) {
            return [];
        }

        return $arguments;
    }
}

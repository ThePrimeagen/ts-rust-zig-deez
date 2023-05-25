<?php

enum TokenType {
    case Illegal;
    case Eof;
    case Ident;
    case Int;
    case Equal;
    case Plus;
    case Comma;
    case Semicolon;
    case LeftParen;
    case RightParen;
    case LeftBrace;
    case RightBrace;
    case Function;
    case Let;
}

readonly class Token {
    public TokenType $type;
    public ?string $literal;

    public function __construct(TokenType $type, string $literal = null) {
        $this->type = $type;
        $this->literal = $literal;
    }
}

class Tokenizer {
    private int $inputLength;
    private string $input;

    private int $position = 0;
    private int $readPosition = 0;
    private ?string $ch;

    private static array $keywords = [
        "fn" => TokenType::Function,
        "let" => TokenType::Let,
    ];

    public function __construct(string $input, ?int $length = null) {
        $this->input = $input;
        if ($length === null) {
            $length = strlen($input);
        }

        $this->inputLength = $length;
        $this->readNextChar();
    }

    public function getNextToken(): Token {
        $this->skipWhitespaces();

        $ch = $this->ch;

        $token = match (true) {
            $ch === "{" => new Token(TokenType::LeftBrace),
            $ch === "}" => new Token(TokenType::RightBrace),
            $ch === "(" => new Token(TokenType::LeftParen),
            $ch === ")" => new Token(TokenType::RightParen),
            $ch === "," => new Token(TokenType::Comma),
            $ch === ";" => new Token(TokenType::Semicolon),
            $ch === "+" => new Token(TokenType::Plus),
            $ch === "=" => new Token(TokenType::Equal),
//            $ch === "$" => new Token(TokenType::Ident, $this->readWord()), // kidding
            $ch === null => new Token(TokenType::Eof),
            Tokenizer::isLetter($ch) => $this->readWordToken(),
            ctype_digit($ch) => new Token(TokenType::Int, $this->readInteger()),
            default => new Token(TokenType::Illegal, $ch),
        };

        if (
            $token->type === TokenType::Function ||
            $token->type === TokenType::Let ||
            $token->type === TokenType::Ident ||
            $token->type === TokenType::Int ||
            $token->type === TokenType::Illegal
        ) {
            return $token;
        }

        $this->readNextChar();

        return $token;
    }

    private function skipWhitespaces(): void {
        while ($this->ch === " " || $this->ch === "\t" || $this->ch === "\n" || $this->ch === "\r") {
            $this->readNextChar();
        }
    }

    private function readNextChar(): void {
        if ($this->readPosition >= $this->inputLength) {
            $this->ch = null;
        } else {
            $this->ch = $this->input[$this->readPosition];
        }

        $this->position = $this->readPosition;
        $this->readPosition++;
    }

    private static function isLetter(string $ch): bool {
        return $ch >= "a" && $ch <= "z" || $ch >= "A" && $ch <= "Z" || $ch === "_";
    }

    private function readWordToken(): Token {
        $word = $this->readWord();

        if (isset(self::$keywords[$word])) {
            return new Token(self::$keywords[$word]);
        }

        return new Token(TokenType::Ident, $word);
    }

    private function readWord(): string {
        $position = $this->position;

        while (Tokenizer::isLetter($this->ch)) {
            $this->readNextChar();
        }

        return substr($this->input, $position, $this->position - $position);
    }

    private function readInteger(): string {
        $position = $this->position;

        while (ctype_digit($this->ch)) {
            $this->readNextChar();
        }

        return substr($this->input, $position, $this->position - $position);
    }
}

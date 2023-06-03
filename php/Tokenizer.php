<?php

enum TokenType {
    case Illegal;
    case Eof;
    case Identifier;
    case Integer;
    case Assign;
    case Plus;
    case Minus;
    case Not;
    case Asterisk;
    case Slash;
    case LessThan;
    case GreaterThan;
    case Comma;
    case Semicolon;
    case LeftParen;
    case RightParen;
    case LeftBrace;
    case RightBrace;
    case Function;
    case Let;
    case True;
    case False;
    case If;
    case Else;
    case Return;
    case Equals;
    case NotEquals;
    case String;
    case UnterminatedString;
}

readonly class Token {

    public function __construct(public TokenType $type,
                                public ?string $literal = null) {
    }
}

class Tokenizer {
    private int $inputLength;
    private string $input;

    private int $position = 0;
    private int $readPosition = 0;
    private string $ch = "\0";

    /** @var TokenType[]  */
    private static array $keywords = [
        "fn" => TokenType::Function,
        "let" => TokenType::Let,
        "true" => TokenType::True,
        "false" => TokenType::False,
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "return" => TokenType::Return,
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
            $ch === "=" => $this->readAssignOrEqualsToken(),
            $ch === "+" => new Token(TokenType::Plus),
            $ch === "-" => new Token(TokenType::Minus),
            $ch === "!" => $this->readNotOrNotEqualsToken(),
            $ch === "*" => new Token(TokenType::Asterisk),
            $ch === "/" => new Token(TokenType::Slash),
            $ch === "<" => new Token(TokenType::LessThan),
            $ch === ">" => new Token(TokenType::GreaterThan),
            $ch === '"' => $this->readStringToken(),
//            $ch === "$" => new Token(TokenType::Ident, $this->readWord()), // kidding
            $ch === "\0" => new Token(TokenType::Eof),
            Tokenizer::isLetter($ch) => $this->readWordToken(),
            ctype_digit($ch) => new Token(TokenType::Integer, $this->readInteger()),
            default => new Token(TokenType::Illegal, $ch),
        };

        if (
            $token->type === TokenType::Function ||
            $token->type === TokenType::Let ||
            $token->type === TokenType::Identifier ||
            $token->type === TokenType::Integer ||
            $token->type === TokenType::True ||
            $token->type === TokenType::False ||
            $token->type === TokenType::If
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

    private function peekChar(): string {
        if ($this->readPosition >= $this->inputLength) {
            return "\0";
        }

        return $this->input[$this->readPosition];
    }

    private function readNextChar(): void {
        if ($this->readPosition >= $this->inputLength) {
            $this->ch = "\0";
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

        return new Token(TokenType::Identifier, $word);
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

    private function readAssignOrEqualsToken(): Token {
        if ($this->peekChar() === "=") {
            $this->readNextChar();
            return new Token(TokenType::Equals);
        }

        return new Token(TokenType::Assign);
    }

    private function readNotOrNotEqualsToken(): Token {
        if ($this->peekChar() === "=") {
            $this->readNextChar();
            return new Token(TokenType::NotEquals);
        }

        return new Token(TokenType::Not);
    }

    private function readStringToken(): Token {
        $startPosition = $this->position + 1;

        $escaped = false;
        $lastIsQuote = false;

        do {
            $this->readNextChar();
            if ($this->ch === '\\') {
                $escaped = true;
                $this->readNextChar();
                if ($this->ch === '"') {
                    $this->readNextChar();
                }
            }
            $lastIsQuote = $this->ch === '"';
        } while (!$lastIsQuote && $this->ch !== "\0");

        if ($escaped) {
            $stringWithQuotes = substr($this->input, $startPosition - 1, $this->position - $startPosition + 2);

            try {
                $stringValue = json_decode(
                    json: $stringWithQuotes,
                    flags: JSON_THROW_ON_ERROR,
                );
            } catch (JsonException) {
                return new Token(TokenType::UnterminatedString, $stringWithQuotes);
            }

            return new Token($lastIsQuote ? TokenType::String : TokenType::UnterminatedString, $stringValue);
        }

        $stringValue = substr($this->input, $startPosition, $this->position - $startPosition);

        if ($lastIsQuote) {
            return new Token(TokenType::String, $stringValue);
        }

        return new Token(TokenType::UnterminatedString, '"' . $stringValue);
    }
}

<?php
enum TokenType: string {
    case Illegal = "ILLEGAL";
    case Eof = "";
    case Ident = "IDENT";
    case Int = "INT";
    case Equal = "=";
    case Plus = "+";
    case Comma = ",";
    case Semicolon = ";";
    case LeftParen = "(";
    case RightParen = ")";
    case LeftBrace = "{";
    case RightBrace = "}";
    case Function = "FUNCTION";
    case Let = "LET";
  //case Phlet = "$";
}

readonly class Token {
    public function __construct(
        public TokenType $type,
        public ?string $literal = null
    ) {}
}

class Tokenizer {
    private string $input;
    private int $position = 0;
    private int $readPosition = 0;
    private ?string $ch;

    private static array $keywords = [
        "fn" => TokenType::Function,
        "let" => TokenType::Let,
    ];

    public function __construct(string $input) {
        $this->input = $input;
        $this->readNextChar();
    }

    public function getNextToken(): Token {
        $this->skipWhitespaces();

        if( $token = TokenType::tryFrom($this->ch) ) {
            $this->readNextChar();
            return new Token($token);
        }

        return match (true) {
            self::isLetter($this->ch) => $this->readWordToken(),
            ctype_digit($this->ch) => new Token(TokenType::Int, $this->readInteger()),
            default => new Token(TokenType::Illegal, $this->ch)
        };
    }

    private function skipWhitespaces(): void {
        while ( ctype_space( $this->ch ) ) {
            $this->readNextChar();
        }
    }

    private function readNextChar(): void {
        $this->position = $this->readPosition;
        $this->ch = $this->input[$this->readPosition++] ?? null;
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

        while (self::isLetter($this->ch)) {
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

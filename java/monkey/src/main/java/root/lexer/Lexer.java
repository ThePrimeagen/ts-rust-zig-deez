package root.lexer;

import root.LocalizedToken;
import root.Token;
import root.TokenType;

import java.util.Map;

public class Lexer {

    private static final Map<Character, String> VALID_ESCAPES = Map.of(
            'n', "\n",
            't', "\t",
            '\\', "\\",
            '"', "\"",
            '\'', "'"
    );

    private final String input;
    private final String[] lines;
    private int pos = 0;
    private int line = 0;
    private int column = 0;

    public Lexer(String input) {
        this.input = input;
        this.lines = input.split("\n");
    }

    public LocalizedToken nextLocalized() {
        this.skipWhitespace();

        var start = this.column;
        var lineStart = this.line;
        var codeLine = lineStart >= this.lines.length ? "" : this.lines[lineStart];

        return this.nextToken().localize(lineStart, start, codeLine);
    }

    public Token nextToken() {
        this.skipWhitespace();

        var currentChar = this.readCc();

        return switch ((Character) currentChar) {
            case '{' -> TokenType.LSQIRLY.token();
            case '}' -> TokenType.RSQIRLY.token();
            case '(' -> TokenType.LPAREN.token();
            case ')' -> TokenType.RPAREN.token();
            case '[' -> TokenType.LBRACKET.token();
            case ']' -> TokenType.RBRACKET.token();
            case ',' -> TokenType.COMMA.token();
            case ';' -> TokenType.SEMI.token();
            case '+' -> TokenType.PLUS.token();
            case '=' -> {
                if (this.getCc() == '=') {
                    this.advance();
                    yield TokenType.EQUAL.token();
                }

                yield TokenType.ASSIGN.token();
            }
            case '-' -> TokenType.MINUS.token();
            case '!' -> {
                if (this.getCc() == '=') {
                    this.advance();
                    yield TokenType.NOT_EQUAL.token();
                }

                yield TokenType.BANG.token();
            }
            case '*' -> TokenType.ASTERISK.token();
            case '/' -> TokenType.SLASH.token();
            case '>' -> TokenType.GT.token();
            case '<' -> TokenType.LT.token();
            case '\0' -> TokenType.EOF.token();
            case '&' -> {
                if (this.getCc() == '&') {
                    this.advance();
                    yield TokenType.AND.token();
                }

                yield TokenType.ILLEGAL.createToken("&");
            }
            case '|' -> {
                if (this.getCc() == '|') {
                    this.advance();
                    yield TokenType.OR.token();
                }

                yield TokenType.ILLEGAL.createToken("|");
            }
            case '"', '\'' -> readString(currentChar);

            case Character c when isLetter(c) -> {
                var ident = this.indent(currentChar);
                yield switch (ident) {
                    case "fn" -> TokenType.FUNC.token();
                    case "let" -> TokenType.LET.token();
                    case "true" -> TokenType.TRUE.token();
                    case "false" -> TokenType.FALSE.token();
                    case "if" -> TokenType.IF.token();
                    case "else" -> TokenType.ELSE.token();
                    case "return" -> TokenType.RETURN.token();
                    case "null" -> TokenType.NULL.token();
                    default -> TokenType.IDENTIFIER.createToken(ident);
                };
            }
            case Character c when Character.isDigit(c) -> TokenType.INT.createToken(this.number(currentChar));
            default -> TokenType.ILLEGAL.createToken(String.valueOf(currentChar));
        };
    }

    private Token readString(Character startingChar) {
        char character;
        var stringBuilder = new StringBuilder();

        while ((character = this.readCc()) != '\0') {
            // Support for escape characters
            if (character == '\\') {
                char slash = character;
                char escaped = this.readCc();

                if (VALID_ESCAPES.containsKey(escaped)) {
                    stringBuilder.append(VALID_ESCAPES.get(escaped));
                    continue;
                } else {
                    stringBuilder.append(slash).append(escaped);
                    // skipping to the end of the string
                    while ((character = this.readCc()) != '\0') {
                        if (character == startingChar) {
                            break;
                        }
                        stringBuilder.append(character);
                    }

                    // TODO maybe create a Lexing exception to better explain errors like this
                    return TokenType.ILLEGAL.createToken(stringBuilder.toString());
                }
            }

            if (character == startingChar) {
                break;
            }

            stringBuilder.append(character);
        }

        String string = stringBuilder.toString();

        if (character != startingChar) {
            return TokenType.ILLEGAL.createToken(string);
        }

        return TokenType.STRING.createToken(string);
    }

    private void advance() {
        if (this.pos >= this.input.length()) {
            return;
        }
        this.pos++;
        this.column++;
    }

    protected char getCc() {
        return this.peek(this.pos);
    }

    private char readCc() {
        var currentChar = this.getCc();
        this.advance();
        return currentChar;
    }

    private char peek(int pos) {
        if (pos >= this.input.length() || pos < 0) {
            return '\0';
        }
        return this.input.charAt(pos);
    }

    private boolean isLetter(char c) {
        return Character.isLetter(c) || c == '_';
    }

    private String number(char firstChar) {
        int pos = this.pos;
        while (Character.isDigit(this.getCc())) {
            this.advance();
        }
        return firstChar + this.input.substring(pos, this.pos);
    }

    private String indent(char firstChar) {
        int pos = this.pos;
        while (this.isLetter(this.getCc())) {
            this.advance();
        }
        return firstChar + this.input.substring(pos, this.pos);
    }

    private void skipWhitespace() {
        while (Character.isWhitespace(this.getCc())) {
            if (this.getCc() == '\n') {
                this.column = -1;
                this.line++;
            }
            this.advance();
        }
    }
}

package the.primeagen.lexer;

import java.util.function.Predicate;

public class DefaultLexerImpl implements ILexer {

    private static final String FUNCTION_IDENTIFIER = "fn";
    private static final String LET_IDENTIFIER = "let";

    private final String m_input;

    private int m_position;

    public DefaultLexerImpl(String input) {
        m_input = input;
    }

    @Override
    public Token getNextToken() {
        skipWhitespace();

        final char c = readCharacter();
        Token token = switch (c) {
            case '=' -> Token.of(Token.Type.EQUAL);
            case '+' -> Token.of(Token.Type.PLUS);
            case ',' -> Token.of(Token.Type.COMMA);
            case ';' -> Token.of(Token.Type.SEMICOLON);
            case '(' -> Token.of(Token.Type.LEFT_PARENTHESES);
            case ')' -> Token.of(Token.Type.RIGHT_PARENTHESES);
            case '{' -> Token.of(Token.Type.LEFT_BRACE);
            case '}' -> Token.of(Token.Type.RIGHT_BRACE);
            case 0 -> Token.of(Token.Type.EOF);
            default -> null;
        };

        if (token != null) {
            m_position++;
            return token;
        }

        if (isIdentifierCharacter(c)) {
            final String identifier = readIdentifier();
            token = switch (identifier) {
                case FUNCTION_IDENTIFIER -> Token.of(Token.Type.FUNCTION);
                case LET_IDENTIFIER -> Token.of(Token.Type.LET);
                default -> Token.of(Token.Type.IDENTIFIER, identifier);
            };

            return token;
        }

        if (isNumberCharacter(c)) {
            return Token.of(Token.Type.INT, readNumber());
        }

        return Token.of(Token.Type.ILLEGAL);
    }

    private char readCharacter() {
        return m_position >= m_input.length()
                ? 0
                : m_input.charAt(m_position);
    }

    private void skipWhitespace() {
        readWhile(Character::isWhitespace);
    }

    private String readIdentifier() {
        return readWhile(this::isIdentifierCharacter);
    }

    private boolean isIdentifierCharacter(char c) {
        return Character.isLetter(c) || c == '_';
    }

    private String readNumber() {
        return readWhile(this::isNumberCharacter);
    }

    private boolean isNumberCharacter(char c) {
        return Character.isDigit(c);
    }

    private String readWhile(Predicate<Character> predicate) {
        final int startPosition = m_position;
        while (predicate.test(readCharacter())) {
            m_position++;
        }
        return m_input.substring(startPosition, m_position);
    }
}

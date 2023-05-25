package the.primeagen.lexer;

import java.util.Objects;

public class Token {

    private final Type m_type;
    private final String m_literal;

    public Token(Type type, String literal) {
        m_type = type;
        m_literal = literal;
    }

    public static Token of(Type type) {
        return new Token(type, null);
    }

    public static Token of(Type type, String literal) {
        return new Token(type, literal);
    }

    public Type getType() {
        return m_type;
    }

    public String getLiteral() {
        return m_literal;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        Token token = (Token) o;
        return m_type == token.m_type && Objects.equals(m_literal, token.m_literal);
    }

    @Override
    public int hashCode() {
        return Objects.hash(m_type, m_literal);
    }

    @Override
    public String toString() {
        return "Token{" +
                "m_type=" + m_type +
                ", m_literal='" + m_literal + '\'' +
                '}';
    }

    public enum Type {
        IDENTIFIER,
        INT,
        EOF,
        ILLEGAL,
        EQUAL,
        PLUS,
        COMMA,
        SEMICOLON,
        LEFT_PARENTHESES,
        RIGHT_PARENTHESES,
        LEFT_BRACE,
        RIGHT_BRACE,
        FUNCTION,
        LET
    }
}

package the.primeagen.lexer;

public interface ILexer {

    /**
     * Parses the next token from the input
     *
     * @return the next token consisting of a type and an optional literal
     */
    Token getNextToken();
}

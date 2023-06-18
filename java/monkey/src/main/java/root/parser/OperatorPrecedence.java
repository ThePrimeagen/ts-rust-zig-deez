package root.parser;

import root.TokenType;

public enum OperatorPrecedence {
    LOWEST,
    EQUALS,         // ==
    LESS_GREATER,   // > or <
    SUM,            // +
    PRODUCT,        // *
    PREFIX,         // -X or !X
    CALL;           // myFunction(X)

    // TODO Should we put this in the TokenType enum?
    public static OperatorPrecedence precedenceForTokenType(TokenType tokenType) {
        return switch (tokenType) {
            case EQUAL, NOT_EQUAL   -> EQUALS;
            case LT, GT             -> LESS_GREATER;
            case PLUS, MINUS        -> SUM;
            case SLASH, ASTERISK    -> PRODUCT;
            default                 -> LOWEST;
        };
    }
}

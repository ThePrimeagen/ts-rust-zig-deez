package root.parser;

import root.TokenType;

public enum OperatorPrecedence {
    LOWEST,
    EQUALS,         // == or !=
    LESS_GREATER,   // > or <
    SUM,            // + or -
    PRODUCT,        // * or /
    PREFIX,         // -X or !X
    CALL;           // myFunction(X)

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

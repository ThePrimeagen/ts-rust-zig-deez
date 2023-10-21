package root.parser;

import root.TokenType;

public enum OperatorPrecedence {
    LOWEST,
    OR,             // ||
    AND,            // &&
    EQUALS,         // == or !=
    LESS_GREATER,   // > or <
    SUM,            // + or -
    PRODUCT,        // * or /
    PREFIX,         // -X or !X
    CALL,           // myFunction(X)
    INDEX;          // array[x]

    public static OperatorPrecedence precedenceForTokenType(TokenType tokenType) {
        return switch (tokenType) {
            case LBRACKET           -> INDEX;
            case LPAREN             -> CALL;
            case EQUAL, NOT_EQUAL   -> EQUALS;
            case LT, GT             -> LESS_GREATER;
            case PLUS, MINUS        -> SUM;
            case SLASH, ASTERISK    -> PRODUCT;
            case OR                 -> OR;
            case AND                -> AND;
            default                 -> LOWEST;
        };
    }
}

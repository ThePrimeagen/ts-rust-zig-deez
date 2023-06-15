package root.parser;

public enum OperatorPrecedence {
    LOWEST,
    EQUALS, // ==
    LESS_GREATER, // > or <
    SUM, //+
    PRODUCT, // *
    PREFIX, // -X or !X
    CALL, // myFunction(X)
}

class Tests {
    public static void main(String[] args) {
        Tests.singleCharacterTokenTests();
    }

    public static void singleCharacterTokenTests() {
        String input = "=+(){},;";
        Lexer l = new Lexer(input);
        TokenType[] expected = {
                TokenType.EQUAL,
                TokenType.PLUS,
                TokenType.LPAREN,
                TokenType.RPAREN,
                TokenType.LSQIRLY,
                TokenType.RSQIRLY,
                TokenType.COMMA,
                TokenType.SEMI,
                TokenType.EOF,
        };

        for (TokenType t : expected) {
            TokenType g = l.nextToken().type();
            if (!t.equals(g)) {
                throw new AssertionError(String.format("Wanted %s, got %s\n", t.name(), g.name()));
            }
        }
    }
}

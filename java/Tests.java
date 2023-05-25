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
        };

        for (TokenType t : expected) {
            boolean equals = t.equals(l.nextToken().type());
            assert (equals);
        }
    }
}

package the.primeagen;

import the.primeagen.lexer.DefaultLexerImpl;
import the.primeagen.lexer.ILexer;
import the.primeagen.lexer.Token;

public class Main {

    public static void main(String[] args) {
        ILexer lexer = new DefaultLexerImpl("Gradle deez");

        Token token;
        do {
            token = lexer.getNextToken();
            System.out.println(token);
        } while (!Token.Type.EOF.equals(token.getType()) || !Token.Type.ILLEGAL.equals(Token.Type.ILLEGAL));
    }
}
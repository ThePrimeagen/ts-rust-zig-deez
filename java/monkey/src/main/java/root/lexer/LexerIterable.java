package root.lexer;

import root.Token;

import java.util.Iterator;

public class LexerIterable implements Iterable<Token> {
    private final Lexer lexer;

    private LexerIterable(Lexer lexer) {
        this.lexer = lexer;
    }

    public static Iterator<Token> fromString(String input) {
        return new LexerIterable(new Lexer(input)).iterator();
    }

    @Override
    public Iterator<Token> iterator() {
        return new LexerIterator(lexer);
    }

    private record LexerIterator(Lexer lexer) implements Iterator<Token> {
        @Override
        public boolean hasNext() {
            return lexer.getCc() != '\0';
        }

        @Override
        public Token next() {
            return lexer.nextToken();
        }
    }
}

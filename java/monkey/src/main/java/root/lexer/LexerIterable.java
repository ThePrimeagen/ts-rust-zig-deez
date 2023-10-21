package root.lexer;

import root.LocalizedToken;

import java.util.Iterator;

class LexerIterable implements Iterable<LocalizedToken> {
    private final Lexer lexer;

    private LexerIterable(Lexer lexer) {
        this.lexer = lexer;
    }

    static Iterator<LocalizedToken> fromLexer(Lexer lexer) {
        return new LexerIterable(lexer).iterator();
    }

    @Override
    public Iterator<LocalizedToken> iterator() {
        return new LexerIterator(lexer);
    }

    record LexerIterator(Lexer lexer) implements Iterator<LocalizedToken> {
        @Override
        public boolean hasNext() {
            return lexer.getCc() != '\0';
        }

        @Override
        public LocalizedToken next() {
            return lexer.nextLocalized();
        }
    }
}

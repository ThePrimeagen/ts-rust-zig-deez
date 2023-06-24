package root.parser;

@FunctionalInterface
public interface ParserSupplier<T> {

    T get() throws ParserException;
}

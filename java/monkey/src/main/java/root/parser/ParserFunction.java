package root.parser;

@FunctionalInterface
public interface ParserFunction<T, R> {

    R apply(T t) throws ParserException;
}

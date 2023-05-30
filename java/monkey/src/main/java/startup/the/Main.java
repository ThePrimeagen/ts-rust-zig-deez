package startup.the;

import startup.the.parsing.Lexer;

public class Main {

    private final static String placeholder = """
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                 x + y;
            };
            let result = add(five, ten);""";

    public static void main(String[] args) {
        new Lexer(placeholder);
    }
}
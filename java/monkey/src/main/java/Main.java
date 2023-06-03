public class Main {

    private final static String placeholder = """
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                 x + y;
            };
            let result = add(five, ten);""";

    public static void main(String[] args) {
        var lexer = new Lexer(placeholder);

        var t = lexer.nextToken();
        while (t.type() != TokenType.EOF) {
            System.out.println(t);
            t = lexer.nextToken();
        }

        System.out.println(t);
    }
}
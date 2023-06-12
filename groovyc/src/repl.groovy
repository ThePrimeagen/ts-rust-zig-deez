class Repl {
    final PROMPT = "8==> ";

    def Start() {
        while (true) {
            print("$PROMPT");
            String line = System.in.newReader().readLine();
            if (!line) return;

            Lexer l = new Lexer(line);
            for (Token tok = l.NextToken(); tok.Type != TokenType.EOF; tok = l.NextToken()) {
                println("{ Type: ${tok.Type}, Literal: ${tok.Literal} }");
            }
        }
    }
}

void main() {
    println("Monkey around with deez nuts ${System.getenv("USER")}");
    mainRepl = new Repl();
    mainRepl.Start();
}

main()

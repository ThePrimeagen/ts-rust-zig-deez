namespace Monkey {

  static void repl() {
    string? line = null;

    do {
      line = stdin.read_line();
      if (line == null) {
        break;
      }
      var l = new Lexer(line);
      Token tok;
      for (tok = l.next_token(); tok.type != TokenType.EOF; tok = l.next_token()){
        print(@"{ type=$(tok.type), literal=$(tok.literal) }\n");
      }
    } while (true);

    print("line=%s", line);
  }

}

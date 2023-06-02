import 'package:dart_deez/dart_deez.dart';
import 'dart:io';

String yellow(String s) => "\x1b[33m$s\x1b[0m";

void exec(String code) {
  final lexer = Lexer(code);
  final parser = Parser(lexer);

  final program = parser.parseProgram();
  if (!parser.checkParserErrors()) {
    print(program);
  }
}

Never repl() {
  final prompt = yellow("(deez)>> ");

  while (true) {
    stdout.write(prompt);
    final line = stdin.readLineSync();

    if (line == null || line == "exit") break;
    if (line.isEmpty) continue;

    exec(line);
  }

  exit(0);
}

void main(List<String> arguments) async {
  if (arguments.isEmpty) {
    print(
      "You are now in REPL mode because you didn't specify a filename.\n"
      "If you want to run a file, specify it as an argument. like so:\n"
      "Press Ctrl+C or type exit to exit.\n",
    );
    repl();
  }

  final fileName = arguments[0];
  final file = File(fileName);

  if (!await file.exists()) {
    print("File not found: $fileName");
    return;
  }

  final contents = await file.readAsString();

  exec(contents);
}

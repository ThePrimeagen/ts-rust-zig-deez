import 'dart:async';
import 'dart:convert';
import 'dart:io';

import 'package:dart_deez/dart_deez.dart';

class REPL {
  REPL({
    String? prompt,
  }) : _prompt = prompt ?? '>> ';

  Future<void> run() async {
    stdout
      ..writeln('Welcome to the Monkey Programming Language')
      ..writeln('Feel free to type in commands');

    try {
      // keep reading the stdin until you receive an 'exit' command
      while (true) {
        stdout.write(_prompt);
        final line = stdin.readLineSync(encoding: utf8);
        switch (line?.trim().toLowerCase()) {
          case null:
            break;
          case 'exit':
            stdout.writeln('Bye!');
            exit(0);
          default:
            final lexer = Tokenizer(line!);
            final tokens = <Token>[];
            var token = lexer.nextToken();
            while (token.type != TokenType.eof) {
              tokens.add(token);
              token = lexer.nextToken();
            }
            for (final element in tokens) {
              stdout.writeln(element.toString());
            }
        }
      }
    } catch (e) {
      stdout.writeln('Error: $e');
    }
  }

  late final String _prompt;
}

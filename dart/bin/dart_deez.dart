import 'package:dart_deez/dart_deez.dart';
import 'dart:io';

void main(List<String> arguments) async {
  if (arguments.isEmpty) {
    print("Usage: dart run bin/main.dart <filename>");
    return;
  }

  final fileName = arguments[0];
  final file = File(fileName);

  if (!await file.exists()) {
    print("File not found: $fileName");
    return;
  }

  final contents = await file.readAsString();

  final lexer = Lexer(contents);

  for (final tok in lexer.iter()) {
    print(tok);
  }
}

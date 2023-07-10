import 'package:monkeydart/monkeydart.dart';

class ParserException implements Exception {
  ParserException(
    this.message,
    this.parser,
    this.statements,
  );

  final String message;
  final Parser parser;
  final List<Statement> statements;

  @override
  String toString() {
    return '🙈-🙉-🙊: $message';
  }
}

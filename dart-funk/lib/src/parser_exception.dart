import 'package:monkeydart/monkeydart.dart';

class ParserException implements Exception {
  ParserException(
    this.message,
    this.parser,
    this.statements, {
    this.line,
    this.column,
  });

  final String message;
  final Parser parser;
  final List<Statement> statements;
  final int? line;
  final int? column;

  @override
  String toString() {
    final positionClause =
        line != null && column != null ? ' at line $line, column $column' : '';
    return 'InterpreterException: $message$positionClause';
  }
}

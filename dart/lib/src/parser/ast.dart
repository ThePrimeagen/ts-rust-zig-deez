import 'package:dart_deez/src/lexer/lexer.dart';

// sealed classes can't be constructed, extended, implemented or mixed in outside of the current library.
// Also helps exhaustiveness checking in switch statements.
sealed class Node {
  final Token token;

  Node(this.token);

  String tokenLiteral() {
    return token.literal;
  }

  @override
  String toString() => "Node(${token.type.name}, ${token.literal})";
}

class Statement extends Node {
  Statement(Token token) : super(token);
}

class Expression extends Node {
  Expression(Token token) : super(token);
}

class ExpressionStatement extends Statement {
  final Expression expression;

  ExpressionStatement(Token token, this.expression) : super(token);

  @override
  String toString() => expression.toString();
}

class Program extends Node {
  final List<Statement> statements;

  Program(this.statements) : super(const Token.eof());

  @override
  String tokenLiteral() {
    if (statements.isNotEmpty) {
      return statements.first.tokenLiteral();
    }
    return "";
  }

  @override
  String toString() => statements.join("\n");
}

// final class is just like a sealed class, but without the exhaustiveness checking.
final class Identifier extends Expression {
  final String value;

  Identifier(Token token, this.value) : super(token);

  @override
  String toString() => "Identifier($value)";
}

class LetStatement extends Statement {
  final Identifier name;
  final Expression value;

  LetStatement(Token token, this.name, this.value) : super(token);

  @override
  String toString() => "${tokenLiteral()} $name = $value;";
}

class ReturnStatement extends Statement {
  final Expression returnValue;

  ReturnStatement(Token token, this.returnValue) : super(token);

  @override
  String toString() => "${tokenLiteral()} $returnValue;";
}

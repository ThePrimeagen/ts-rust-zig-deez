import 'package:meta/meta.dart';
import 'package:monkeydart/monkeydart.dart';

@immutable

/// Basic element of the AST
sealed class Node {
  const Node(this.token);
  final Token token;
  String tokenLiteral() {
    return token.value;
  }

  @override
  String toString() {
    return '${token.type.name} ${token.value}';
  }
}

/// Statements do not return values
class Statement extends Node {
  const Statement(super.token);

  @override
  String toString() {
    return token.value;
  }
}

/// Expressions return values
class Expression extends Node {
  const Expression(super.token);
}

class Identifier extends Expression {
  Identifier(this.value) : super(Token.ident(value));
  final String value;

  @override
  String toString() {
    return value;
  }
}

class LetStatement extends Statement {
  const LetStatement(this.name, this.value) : super(const Token.let());
  final Identifier name;
  final Expression value;

  @override
  String toString() {
    return '${token.type.name} ${name.value} = ${value.token.value};';
  }
}

class ReturnStatement extends Statement {
  const ReturnStatement(this.returnValue) : super(const Token.return_());
  final Expression returnValue;

  @override
  String toString() {
    return '${token.type} ${returnValue.token.value};';
  }
}

class ExpressionStatement extends Statement {
  ExpressionStatement(this.expression) : super(expression.token);
  final Expression expression;

  @override
  String toString() {
    final retVal = StringBuffer(expression);
    return retVal.toString();
  }
}

class PrefixExpression extends Expression {
  const PrefixExpression(super.token, this.operator, this.right);
  final String operator;
  final Expression right;

  @override
  String toString() {
    return '($operator$right)';
  }
}

class InfixExpression extends Expression {
  const InfixExpression(
    super.token,
    this.left,
    this.operator,
    this.right,
  );
  final Expression left;
  final String operator;
  final Expression right;

  @override
  String tokenLiteral() {
    return token.value;
  }

  @override
  String toString() {
    return '($left $operator $right)';
  }
}

class Program extends Node {
  Program() : super(const Token.eof());
  final List<Statement> statements = [];

  @override
  String tokenLiteral() {
    if (statements.isNotEmpty) {
      return statements.first.token.value;
    }
    return '';
  }

  @override
  String toString() {
    return statements.join();
  }
}

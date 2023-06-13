import 'package:meta/meta.dart';
import 'package:monkeydart/monkeydart.dart';

@immutable

/// Basic element of the AST
sealed class Node {
  const Node(this.token);
  final Token token;

  String tokenLiteral() {
    return literalToken(this);
  }

  @override
  String toString() {
    return '${token.type.name} ${token.value}';
  }
}

/// Statements do not return values
class Statement extends Node {
  const Statement(super.token);
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

class IntegerLiteral extends Expression {
  IntegerLiteral(this.value) : super(Token.int(value.toString()));
  final int value;
  @override
  String toString() {
    return token.value;
  }
}

class BooleanLiteral extends Expression {
  const BooleanLiteral({required this.value})
      : super(value ? const Token.true_() : const Token.false_());
  final bool value;

  @override
  String toString() {
    return token.value;
  }
}

class IfExpression extends Expression {
  const IfExpression({
    required this.condition,
    required this.consequence,
    this.alternative,
  }) : super(const Token.if_());
  final Expression condition;
  final BlockStatement consequence;
  final BlockStatement? alternative;

  @override
  String toString() {
    final retVal = StringBuffer(token.value)
      ..write('($condition) ')
      ..write(consequence);
    if (alternative != null) {
      retVal.write(' else $alternative');
    }
    return retVal.toString();
  }
}

class FunctionLiteral extends Expression {
  const FunctionLiteral({
    required this.parameters,
    required this.body,
  }) : super(const Token.function());
  final List<Identifier> parameters;
  final BlockStatement body;

  @override
  String toString() {
    final retVal = StringBuffer(token.value)
      ..write('(')
      ..write(parameters.join(', '))
      ..write(') ')
      ..write(body);
    return retVal.toString();
  }
}

class LetStatement extends Statement {
  const LetStatement(this.name, this.value) : super(const Token.let());
  final Identifier name;
  final Expression value;

  @override
  String toString() {
    return '${token.value} ${name.value} = ${value.token.value};';
  }
}

class ReturnStatement extends Statement {
  const ReturnStatement(this.returnValue) : super(const Token.return_());
  final Expression returnValue;

  @override
  String toString() {
    return '${token.value} ${returnValue.token.value};';
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

class BlockStatement extends Statement {
  const BlockStatement(this.statements) : super(const Token.lSquirly());
  final List<Statement> statements;

  @override
  String toString() {
    final retVal = StringBuffer('${token.value} ');
    for (final statement in statements) {
      retVal.write(statement);
    }
    retVal.write(' ${const Token.rSquirly().value}');
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
  String toString() {
    return '($left $operator $right)';
  }
}

class Program extends Node {
  const Program(this.statements, {this.errors = const []})
      : super(const Token.eof());

  final List<Statement> statements;
  final List<ParserException> errors;

  @override
  String toString() {
    final retVal = StringBuffer('Program:');
    if (statements.isNotEmpty) {
      retVal.write('\n\t${statements.join('\n\t')}');
    }else{
      retVal.write('\n\t\tNo statements found');
    }
    if (errors.isNotEmpty) {
      retVal.write('\n\tErrors:\n\t${errors.join('\n\t')}');
    }
    return retVal.toString();
  }
}

class NullStatement extends Statement {
  const NullStatement() : super(const Token.illegal());

  @override
  String toString() {
    return 'NullStatement';
  }
}

class NullExpression extends Expression {
  const NullExpression() : super(const Token.illegal());

  @override
  String toString() {
    return 'NullExpression';
  }
}

class NullProgram extends Program {
  const NullProgram() : super(const []);

  @override
  String toString() {
    return 'NullProgram';
  }
}

class NullIdentifier extends Identifier {
  NullIdentifier() : super('');

  @override
  String toString() {
    return 'NullIdentifier';
  }
}

String literalToken(Node node) {
  switch (node) {
    case _ when node is Program:
      return 'Program';
    case _ when node is Statement:
      return 'Statement ${node.token.value}';
    case _ when node is Expression:
      return 'Expression ${node.token.value}';
    // case Identifier:
    //   return 'Identifier';
    // case IntegerLiteral:
    //   return 'IntegerLiteral';
    // case PrefixExpression:
    //   return 'PrefixExpression';
    // case InfixExpression:
    //   return 'InfixExpression';
    // case Boolean:
    //   return 'Boolean';
    // case IfExpression:
    //   return 'IfExpression';
    // case BlockStatement:
    //   return 'BlockStatement';
    // case FunctionLiteral:
    //   return 'FunctionLiteral';
    // case CallExpression:
    //   return 'CallExpression';
    default:
      return 'Unknown';
  }
}

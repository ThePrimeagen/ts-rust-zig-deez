import 'package:mason_logger/mason_logger.dart';
import 'package:monkeydart/monkeydart.dart';
import 'package:test/test.dart';

void main() {
  group('AST Node', () {
    late Logger logger;
    setUp(() {
      logger = Logger();
    });
    test(
      'should return valid Node when instantiated',
      () async {
        // arrange

        // act
        const stmt = Statement(Token.let());
        logger
          ..info('toString() $stmt')
          ..info('tokenLiteral() ${stmt.tokenLiteral()}');

        const expr = Expression(Token.let());
        logger
          ..info('toString() $expr')
          ..info('tokenLiteral() ${expr.tokenLiteral()}');

        // assert
        expect(stmt, isA<Statement>());
        expect(stmt.tokenLiteral(), equals('Statement let'));
        expect(expr, isA<Expression>());
        expect(expr.tokenLiteral(), equals('Expression let'));
      },
    );

    test(
      'should return valid Identifier when instantiated',
      () async {
        // arrange

        // act
        final ident = Identifier('myVar');
        logger
          ..info('toString() $ident')
          ..info('tokenLiteral() ${ident.tokenLiteral()}');

        // assert
        expect(ident, isA<Identifier>());
        expect(ident.tokenLiteral(), equals('Expression myVar'));
        expect(ident.value, equals('myVar'));
      },
    );
    test(
      'should return valid LetStatement when instantiated',
      () async {
        // arrange

        // act
        final letStatement = LetStatement(
          Identifier('myVar'),
          Identifier('anotherVar'),
        );
        logger
          ..info('toString() $letStatement')
          ..info('tokenLiteral() ${letStatement.tokenLiteral()}')
          ..info('')
          ..info('...name: ${letStatement.name.tokenLiteral()}')
          ..info('...value: ${letStatement.value.tokenLiteral()}');

        // assert
        expect(letStatement, isA<LetStatement>());
        expect(letStatement.tokenLiteral(), equals('Statement let'));
        expect(letStatement.name.tokenLiteral(), equals('Expression myVar'));
        expect(letStatement.name.value, equals('myVar'));
        expect(
          letStatement.value.tokenLiteral(),
          equals('Expression anotherVar'),
        );
        expect(letStatement.value.token.value, equals('anotherVar'));
      },
    );
    test(
      'should return valid ReturnStatement when instantiated',
      () async {
        // arrange

        // act
        final returnStatement = ReturnStatement(Identifier('myVar'));
        logger
          ..info('toString() $returnStatement')
          ..info('tokenLiteral() ${returnStatement.tokenLiteral()}')
          ..info('')
          ..info(
            '...returnValue: ${returnStatement.returnValue.tokenLiteral()}',
          );

        // assert
        expect(returnStatement, isA<ReturnStatement>());
        expect(returnStatement.tokenLiteral(), equals('Statement return'));
        expect(
          returnStatement.returnValue.tokenLiteral(),
          equals('Expression myVar'),
        );
        expect(
          (returnStatement.returnValue as Identifier).value,
          equals('myVar'),
        );
      },
    );
    test(
      'should return valid ExpressionStatement when instantiated',
      () async {
        // arrange

        // act
        final expressionStatement = ExpressionStatement(Identifier('myVar'));
        logger
          ..info('toString() $expressionStatement')
          ..info('tokenLiteral() ${expressionStatement.tokenLiteral()}')
          ..info('')
          ..info(
            '...expression: ${expressionStatement.expression.tokenLiteral()}',
          );

        // assert
        expect(expressionStatement, isA<ExpressionStatement>());
        expect(expressionStatement.tokenLiteral(), equals('Statement myVar'));
        expect(
          expressionStatement.expression.tokenLiteral(),
          equals('Expression myVar'),
        );
        expect(
          (expressionStatement.expression as Identifier).value,
          equals('myVar'),
        );
      },
    );
    test(
      'should return a Program when instantiated',
      () async {
        // arrange

        // act
        const program = Program([]);
        logger
          ..info('toString() $program')
          ..info('tokenLiteral() ${program.tokenLiteral()}')
          ..info('')
          ..info('...statements: ${program.statements}');

        // assert
        expect(program, isA<Program>());
        expect(program.statements, isEmpty);
      },
    );
    test(
      'should return a valid InfixExpression when instantiated',
      () async {
        // arrange

        // act
        final infixExpression = InfixExpression(
          const Token.return_(),
          const Expression(Token.ident('myVar')),
          '+',
          Identifier('anotherVar'),
        );
        logger
          ..info('toString() $infixExpression')
          ..info('tokenLiteral() ${infixExpression.tokenLiteral()}')
          ..info('')
          ..info('...left: ${infixExpression.left.tokenLiteral()}')
          ..info('...operator: ${infixExpression.operator}')
          ..info('...right: ${infixExpression.right.tokenLiteral()}');

        // assert
        expect(infixExpression, isA<InfixExpression>());
        expect(infixExpression.tokenLiteral(), equals('Expression return'));
        expect(infixExpression.left.tokenLiteral(), equals('Expression myVar'));
        expect(infixExpression.operator, equals('+'));
        expect(
          infixExpression.right.tokenLiteral(),
          equals('Expression anotherVar'),
        );
        expect(
          (infixExpression.right as Identifier).value,
          equals('anotherVar'),
        );
      },
    );
    test(
      'should return a valid PrefixExpression when instantiated',
      () async {
        // arrange

        // act
        final prefixExpression = PrefixExpression(
          const Token.return_(),
          '-',
          Identifier('myVar'),
        );
        logger
          ..info('toString() $prefixExpression')
          ..info('tokenLiteral() ${prefixExpression.tokenLiteral()}')
          ..info('')
          ..info('...operator: ${prefixExpression.operator}')
          ..info('...right: ${prefixExpression.right.tokenLiteral()}');

        // assert
        expect(prefixExpression, isA<PrefixExpression>());
        expect(prefixExpression.tokenLiteral(), equals('Expression return'));
        expect(prefixExpression.operator, equals('-'));
        expect(
          prefixExpression.right.tokenLiteral(),
          equals('Expression myVar'),
        );
        expect(
          (prefixExpression.right as Identifier).value,
          equals('myVar'),
        );
      },
    );
  });
}

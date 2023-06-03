import 'package:mason_logger/mason_logger.dart';
import 'package:monkeydart/monkeydart.dart';
import 'package:test/test.dart';

void main() {
  group('Token', () {
    late Logger logger;
    setUp(() {
      logger = Logger();
    });
    test(
      'should return typeOf Token when instantiated',
      () async {
        // arrange
        const tokens = [
          Token.let(),
          Token.function(),
          Token.true_(),
          Token.false_(),
          Token.if_(),
          Token.else_(),
          Token.return_(),
          Token.assign(),
          Token.equal(),
          Token.notEqual(),
          Token.plus(),
          Token.dash(),
          Token.bang(),
          Token.asterisk(),
          Token.slash(),
          Token.lessThan(),
          Token.greaterThan(),
          Token.comma(),
          Token.semicolon(),
          Token.lSquirly(),
          Token.rSquirly(),
          Token.lParen(),
          Token.rParen(),
          Token.int('1'),
          Token.ident('ident'),
          Token.eof(),
          Token.illegal(),
        ];

        // act

        // assert
        for (final token in tokens) {
          logger.info(token.toString());
          expect(token, isA<Token>());
        }
      },
    );
    test(
      'should return yadda when Token.ident(yadda) is instantiated',
      () async {
        // arrange
        const val = 'yadda';

        // act
        const token = Token.ident(val);
        final tStr = token.toString();

        // assert
        expect(token.value, val);
        expect(token.type, TokenType.ident);
        expect(tStr, '<ident> => $val');
      },
    );
  });
}

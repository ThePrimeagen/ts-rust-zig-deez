import 'package:mason_logger/mason_logger.dart';
import 'package:monkeydart/monkeydart.dart';

final pLogger = Logger(level: Level.debug);

enum Precedence {
  // ignore: unused_field
  _, // not a real precedent
  lowest,
  equals, // ==
  lessGreater, // > or <
  sum, // +
  product, // *
  prefix, // -X or !X
  call, // myFunction(X)
}

Map<TokenType, Precedence> precedences() {
  return {
    TokenType.eq: Precedence.equals,
    TokenType.ne: Precedence.equals,
    TokenType.lt: Precedence.lessGreater,
    TokenType.gt: Precedence.lessGreater,
    TokenType.plus: Precedence.sum,
    TokenType.dash: Precedence.sum,
    TokenType.slash: Precedence.product,
    TokenType.asterisk: Precedence.product,
    TokenType.lParen: Precedence.call,
  };
}

Precedence currentPrecedence(Parser parser) {
  return precedences()[parser.currToken.type] ?? Precedence.lowest;
}

Precedence peekPrecedence(Parser parser) {
  return precedences()[parser.peekToken.type] ?? Precedence.lowest;
}

extension PrecedenceExt on Precedence {
  bool operator >=(Precedence other) {
    return index >= other.index;
  }

  bool operator <(Precedence other) {
    return index < other.index;
  }
}

bool isCurrGtePeek(Parser parser, Precedence currPrec) {
  // pLogger.detail(
  //   'isCurrGtePeek: $currPrec >= ${peekPrecedence(parser)}',
  // );
  return currPrec >= peekPrecedence(parser);
}

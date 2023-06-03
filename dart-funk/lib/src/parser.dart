import 'package:fpdart/fpdart.dart';
import 'package:meta/meta.dart';
import 'package:monkeydart/monkeydart.dart';

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
  };
}

@immutable
class Parser {
  factory Parser(Lexer lexer) {
    final (currLex, currToken) = nextToken(lexer);
    final (_, peekToken) = nextToken(currLex);
    return Parser._(
      lexer,
      Some(currToken),
      Some(peekToken),
    );
  }
  const Parser._(
    this.lexer,
    this.currToken,
    this.peekToken,
  );

  final Lexer lexer;
  final Option<Token> currToken;
  final Option<Token> peekToken;
}



import 'package:equatable/equatable.dart';
import 'package:meta/meta.dart';

@immutable
class Token extends Equatable {
  // variable values
  const Token.ident(this.value) : type = TokenType.ident;
  const Token.int(this.value) : type = TokenType.int;

  // keywords
  const Token.let()
      : type = TokenType.let,
        value = 'let';
  const Token.function()
      : type = TokenType.function,
        value = 'fn';
  const Token.true_()
      : type = TokenType.true_,
        value = 'true';
  const Token.false_()
      : type = TokenType.false_,
        value = 'false';
  const Token.if_()
      : type = TokenType.if_,
        value = 'if';
  const Token.else_()
      : type = TokenType.else_,
        value = 'else';
  const Token.return_()
      : type = TokenType.return_,
        value = 'return';
  // end of things
  const Token.illegal()
      : type = TokenType.illegal,
        value = r'🤷 ¯\_(ツ)_/¯ 🤷';
  const Token.eof()
      : type = TokenType.eof,
        value = '🛑';
  // operators
  const Token.assign()
      : type = TokenType.assign,
        value = '=';
  const Token.equal()
      : type = TokenType.eq,
        value = '==';
  const Token.notEqual()
      : type = TokenType.ne,
        value = '!=';
  const Token.plus()
      : type = TokenType.plus,
        value = '+';
  const Token.dash()
      : type = TokenType.dash,
        value = '-';
  const Token.bang()
      : type = TokenType.bang,
        value = '!';
  const Token.asterisk()
      : type = TokenType.asterisk,
        value = '*';
  const Token.slash()
      : type = TokenType.slash,
        value = '/';
  const Token.greaterThan()
      : type = TokenType.gt,
        value = '>';
  const Token.lessThan()
      : type = TokenType.lt,
        value = '<';
  // separators
  const Token.comma()
      : type = TokenType.comma,
        value = ',';
  const Token.semicolon()
      : type = TokenType.semicolon,
        value = ';';
  const Token.lParen()
      : type = TokenType.lParen,
        value = '(';
  const Token.rParen()
      : type = TokenType.rParen,
        value = ')';
  const Token.lSquirly()
      : type = TokenType.lSquirly,
        value = '{';
  const Token.rSquirly()
      : type = TokenType.rSquirly,
        value = '}';

  final String value;
  final TokenType type;

  static Token identity(String val) {
    switch (val) {
      case 'let':
        return const Token.let();
      case 'fn':
        return const Token.function();
      case 'true':
        return const Token.true_();
      case 'false':
        return const Token.false_();
      case 'if':
        return const Token.if_();
      case 'else':
        return const Token.else_();
      case 'return':
        return const Token.return_();
      default:
        return Token.ident(val);
    }
  }

  @override
  String toString() => '<${type.name}> => $value';

  @override
  List<Object?> get props => [type, value];
}

enum TokenType {
  ident,
  int,
  let,
  illegal,
  eof,
  assign,
  plus,
  comma,
  semicolon,
  lParen,
  rParen,
  lSquirly,
  rSquirly,
  function,
  dash,
  bang,
  asterisk,
  slash,
  lt,
  gt,
  true_,
  false_,
  if_,
  else_,
  return_,
  ne,
  eq,
}

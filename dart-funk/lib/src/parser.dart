import 'package:mason_logger/mason_logger.dart';
import 'package:meta/meta.dart';
import 'package:monkeydart/monkeydart.dart';

final logger = Logger();

final _prefixParseFns = <TokenType, (Parser, Expression) Function(Parser)>{
  TokenType.int: _parseIntegerLiteral,
  TokenType.ident: _parseIdentifier,
  TokenType.bang: _parsePrefixExpression,
  TokenType.dash: _parsePrefixExpression,
  TokenType.plus: _parsePrefixExpression,
  TokenType.asterisk: _parsePrefixExpression,
  TokenType.slash: _parsePrefixExpression,
  TokenType.eq: _parsePrefixExpression,
  TokenType.ne: _parsePrefixExpression,
  TokenType.lt: _parsePrefixExpression,
  TokenType.gt: _parsePrefixExpression,
};

final _infixParseFns =
    <TokenType, (Parser, Expression) Function(Parser, Expression)>{
  TokenType.plus: _parseInfixExpression,
  TokenType.dash: _parseInfixExpression,
  TokenType.asterisk: _parseInfixExpression,
  TokenType.slash: _parseInfixExpression,
  TokenType.eq: _parseInfixExpression,
  TokenType.ne: _parseInfixExpression,
  TokenType.lt: _parseInfixExpression,
  TokenType.gt: _parseInfixExpression,
};

@immutable
class Parser {
  factory Parser(Iterable<Token> tokens, {int tokenIndex = 0}) {
    if (tokens.isEmpty || tokenIndex >= tokens.length) {
      return const NullParser();
    } else {
      final currToken = tokenIndex < tokens.length
          ? tokens.elementAt(tokenIndex)
          : const Token.illegal();
      final peekToken = tokenIndex + 1 < tokens.length
          ? tokens.elementAt(tokenIndex + 1)
          : const Token.illegal();
      return Parser._(
        tokens,
        tokenIndex,
        currToken,
        peekToken,
        const [],
      );
    }
  }

  factory Parser.fromLexer(Lexer lexer) {
    return Parser(
      tokenGenerator(lexer.source),
    );
  }

  factory Parser.fromSource(String source) {
    return Parser(
      tokenGenerator(source),
    );
  }

  const Parser._(
    this.tokens,
    this.tokenIndex,
    this.currToken,
    this.peekToken,
    this.errors,
  );

  final Iterable<Token> tokens;
  final int tokenIndex;
  final Token currToken;
  final Token peekToken;
  final List<ParserException> errors;

  Parser copyWith({
    Iterable<Token>? tokens,
    int? tokenIndex,
    Token? currToken,
    Token? peekToken,
    List<ParserException>? errors,
  }) {
    return Parser._(
      tokens ?? this.tokens,
      tokenIndex ?? this.tokenIndex,
      currToken ?? this.currToken,
      peekToken ?? this.peekToken,
      errors ?? this.errors,
    );
  }

  @override
  String toString() {
    final retVal = StringBuffer('Parser{')
      ..write('Tokens: ${tokenIndex + 1} of ${tokens.length}, ')
      ..write('\n\tcurrToken: $currToken, ')
      ..write('\n\tpeekToken: $peekToken');
    if (errors.isNotEmpty) {
      retVal.write('\n\tErrors: $errors');
    }
    retVal.write('}');
    return retVal.toString();
  }
}

class NullParser extends Parser {
  const NullParser()
      : super._(
          const [],
          0,
          const Token.illegal(),
          const Token.illegal(),
          const [],
        );
}

Parser advanceParser(Parser parser) {
  if (parser.tokenIndex + 1 < parser.tokens.length) {
    final currToken = parser.peekToken;
    final peekToken = parser.tokenIndex + 2 < parser.tokens.length
        ? parser.tokens.elementAt(parser.tokenIndex + 2)
        : const Token.illegal();
    return parser.copyWith(
      tokenIndex: parser.tokenIndex + 1,
      currToken: currToken,
      peekToken: peekToken,
    );
  } else {
    final newError = ParserException(
      'Unexpected end of input',
      parser,
      const [],
      // line: parser.currToken.line,
      // column: parser.currToken.column,
    );
    return parser.copyWith(
      tokenIndex: parser.tokens.length - 1,
      errors: [...parser.errors, newError],
    );
  }
}

Program parse(Parser parser) {
  final (newParser, statements) = _parse(parser, []);
  return Program(statements, errors: newParser.errors);
}

(Parser parser, List<Statement> statements) _parse(
  Parser parser,
  List<Statement> statements,
) {
  switch (parser.currToken) {
    case const Token.eof():
      return (parser, statements);
    case _:
      final (newParser, statement) = _parseStatement(parser);
      return switch (statement) {
        const NullStatement() => _parse(newParser, statements),
        _ => _parse(newParser, [...statements, statement]),
      };
  }
}

(Parser parser, Statement statement) _parseStatement(Parser parser) {
  return switch (parser.currToken.type) {
    TokenType.let => _parseLetStatement(parser),
    TokenType.return_ => _parseReturnStatement(parser),
    _ => _parseExpressionStatement(parser),
  };
}

(Parser parser, Statement statement) _parseExpressionStatement(Parser parser) {
  final (exprParser, expression) = _parseExpression(parser, Precedence.lowest);
  final newParser = _eatNextSemicolon(exprParser);
  return (newParser, ExpressionStatement(expression));
}

(Parser parser, Statement statement) _parseLetStatement(Parser parser) {
  final (letParser, ok) = _expectPeek(parser, TokenType.ident);
  if (!ok) {
    final newParser = _eatNextSemicolon(letParser);
    return (newParser, const NullStatement());
  }
  final (idParser, identifier as Identifier) = _parseIdentifier(letParser);
  final (assParser, assOk) = _expectPeek(idParser, TokenType.assign);
  if (!assOk) {
    final newParser = _eatNextSemicolon(assParser);
    return (newParser, const NullStatement());
  }
  final exprParser = advanceParser(assParser);
  final (remainderParser, expression) =
      _parseExpression(exprParser, Precedence.lowest);
  final newParser = _eatNextSemicolon(remainderParser);
  return (newParser, LetStatement(identifier, expression));
}

(Parser parser, Statement statement) _parseReturnStatement(Parser parser) {
  final retParser = advanceParser(parser);
  final (exprParser, expression) =
      _parseExpression(retParser, Precedence.lowest);
  final stmt = ReturnStatement(expression);
  final newParser = _eatNextSemicolon(exprParser);
  return (newParser, stmt);
}

(Parser parser, bool ok) _expectPeek(Parser parser, TokenType type) {
  return switch (parser.peekToken.type) {
    _ when parser.peekToken.type == type => (advanceParser(parser), true),
    _ => (
        advanceParser(parser).copyWith(
          errors: [
            ParserException(
              'Expected next token to be $type, '
              'got ${parser.peekToken.type} instead',
              parser,
              [],
              // line: parser.peekToken.line,
              // column: parser.peekToken.column,
            )
          ],
        ),
        false
      ),
  };
}

(Parser parser, Expression expression) _parseExpression(
  Parser parser,
  Precedence precedence,
) {
  final prefix = _prefixParseFns[parser.currToken.type];
  if (prefix == null) {
    return (
      parser.copyWith(
        errors: [
          ParserException(
            'No prefix parse function for ${parser.currToken.type}',
            parser,
            [],
            // line: parser.currToken.line,
            // column: parser.currToken.column,
          )
        ],
      ),
      const NullExpression()
    );
  }
  final (exprParser, leftExpr) = prefix(parser);
  return __parseExpression(exprParser, leftExpr, precedence: precedence);
}

(Parser parser, Expression expression) __parseExpression(
  Parser parser,
  Expression leftExp, {
  Precedence precedence = Precedence.lowest,
}) {
  if (_peekTokenIs(parser, TokenType.semicolon) ||
      precedence >= peekPrecedence(parser)) {
    return (parser, leftExp);
  } else {
    final infix = _infixParseFns[parser.peekToken.type];
    if (infix == null) {
      return (parser, leftExp);
    }
    final advParser = advanceParser(parser);
    final (infixParser, newLeft) = infix(advParser, leftExp);
    return __parseExpression(
      infixParser,
      newLeft,
      precedence: precedence,
    );
  }
}

(Parser parser, Expression expression) _parseIntegerLiteral(Parser parser) {
  final trial = int.tryParse(parser.currToken.value);
  if (trial == null) {
    return (
      parser.copyWith(
        errors: [
          ParserException(
            'Unable to parse integer literal ${parser.currToken.value}',
            parser,
            [],
            // line: parser.currToken.line,
            // column: parser.currToken.column,
          )
        ],
      ),
      const NullExpression()
    );
  }
  return (parser, IntegerLiteral(trial));
}

(Parser parser, Expression expression) _parseIdentifier(Parser parser) {
  return (parser, Identifier(parser.currToken.value));
}

Parser _eatNextSemicolon(Parser parser) {
  return switch (parser.currToken.type) {
    TokenType.semicolon => advanceParser(parser),
    TokenType.eof => parser.copyWith(
        errors: [
          ParserException(
            'Unexpected end of input',
            parser,
            [],
            // line: parser.peekToken.line,
            // column: parser.peekToken.column,
          )
        ],
      ),
    TokenType.illegal => parser.copyWith(
        errors: [
          ParserException(
            'Encountered an illegal token in statement',
            parser,
            [],
            // line: parser.peekToken.line,
            // column: parser.peekToken.column,
          )
        ],
      ),
    _ => _eatNextSemicolon(advanceParser(parser)),
  };
}

(Parser parser, Expression expression) _parsePrefixExpression(Parser parser) {
  final operator = parser.currToken.value;
  final advParser = advanceParser(parser);
  final (exprParser, right) = _parseExpression(advParser, Precedence.prefix);
  return (exprParser, PrefixExpression(parser.currToken, operator, right));
}

(Parser parser, Expression expression) _parseInfixExpression(
  Parser parser,
  Expression left,
) {
  final operator = parser.currToken.value;
  final precedence = currentPrecedence(parser);
  final advParser = advanceParser(parser);
  final (exprParser, right) = _parseExpression(advParser, precedence);
  return (exprParser, InfixExpression(parser.currToken, left, operator, right));
}

bool _peekTokenIs(Parser parser, TokenType type) {
  return parser.peekToken.type == type;
}

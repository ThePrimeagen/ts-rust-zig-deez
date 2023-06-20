import 'package:mason_logger/mason_logger.dart';
import 'package:meta/meta.dart';
import 'package:monkeydart/monkeydart.dart';

final logger = Logger(level: Level.warning);

final _prefixParseFns = <TokenType, (Parser, Expression) Function(Parser)>{
  TokenType.int: _parseIntegerLiteral,
  TokenType.ident: _parseExpressionIdentifier,
  TokenType.bang: _parsePrefixExpression,
  TokenType.dash: _parsePrefixExpression,
  TokenType.plus: _parsePrefixExpression,
  TokenType.asterisk: _parsePrefixExpression,
  TokenType.slash: _parsePrefixExpression,
  TokenType.eq: _parsePrefixExpression,
  TokenType.ne: _parsePrefixExpression,
  TokenType.lt: _parsePrefixExpression,
  TokenType.gt: _parsePrefixExpression,
  TokenType.lParen: _parseGroupedExpression,
  TokenType.if_: _parseIfExpression,
  TokenType.function: _parseFunctionLiteral,
  TokenType.string: _parseStringLiteral,
  // TokenType.lSquirly: _parseArrayLiteral,
  // TokenType.lSquirly: _parseHashLiteral,
  TokenType.true_: _parseBoolean,
  TokenType.false_: _parseBoolean,
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
  TokenType.lParen: _parseCallExpression,
};

(Parser parser, bool ok) expectPeek(Parser parser, TokenType type) {
  return switch (parser.peekToken.type) {
    _ when parser.peekToken.type == type => (advanceParser(parser), true),
    TokenType.eof => (
        parser.copyWith(
          errors: [ParserException('No peek token available', parser, [])],
        ),
        false
      ),
    _ => (
        parser.copyWith(
          errors: [
            ParserException(
              'Expected next token to be ${type.name}, '
              'got ${parser.peekToken.type.name} instead',
              parser,
              [],
            )
          ],
        ),
        false
      ),
  };
}

bool _peekTokenIs(Parser parser, TokenType type) {
  return parser.peekToken.type == type;
}

bool _currTokenIs(Parser parser, TokenType type) {
  return parser.currToken.type == type;
}

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
      errors != null ? [...this.errors, ...errors] : this.errors,
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
        // Bail on first error
        const NullStatement() => (newParser, statements),
        _ => _parse(advanceParser(newParser), [...statements, statement]),
      };
  }
}

(Parser parser, Statement statement) _parseStatement(Parser parser) {
  final retVal = switch (parser.currToken.type) {
    TokenType.let => _parseLetStatement(parser),
    TokenType.return_ => _parseReturnStatement(parser),
    TokenType.illegal => (
        parser.copyWith(
          errors: [
            ParserException(
              'Encountered an unexpected token in statement',
              parser,
              [],
            )
          ],
        ),
        const NullStatement()
      ),
    TokenType.eof => (
        parser.copyWith(
          errors: [ParserException('Unexpected end of input', parser, [])],
        ),
        const NullStatement()
      ),
    _ => _parseExpressionStatement(parser),
  };
  // make sure we're at the end of the statement
  return retVal;
}

(Parser parser, Statement statement) _parseExpressionStatement(Parser parser) {
  var newParser = parser;

  final (exprParser, expression) = _parseExpression(parser, Precedence.lowest);
  if (_peekTokenIs(exprParser, TokenType.semicolon)) {
    newParser = eatLastSemicolon(exprParser);
    // newParser = advanceParser(advanceParser(exprParser));
    return (newParser, ExpressionStatement(expression));
  }
  newParser = _currTokenIs(exprParser, TokenType.eof)
      ? exprParser
      : eatLastSemicolon(exprParser);

  return (newParser, ExpressionStatement(expression));
}

(Parser parser, Statement statement) _parseLetStatement(Parser parser) {
  final (idParser, identifier) = _parseIdentifier(parser);
  if (identifier == const NullExpression()) {
    // final nextStmtParser = eatLastSemicolon(idParser, recurse: true);
    return (idParser, const NullStatement());
  }
  final (assParser, assOk) = expectPeek(idParser, TokenType.assign);
  if (!assOk) {
    // final newParser = eatLastSemicolon(assParser);
    return (assParser, const NullStatement());
  }
  final exprParser = advanceParser(assParser);
  final (postExprParser, expression) =
      _parseExpression(exprParser, Precedence.lowest);
  final returnParser = eatLastSemicolon(postExprParser);
  // make sure we're at the end of the statement
  return (returnParser, LetStatement(identifier as Identifier, expression));
}

(Parser parser, Statement statement) _parseReturnStatement(Parser parser) {
  final retParser = advanceParser(parser);
  final (exprParser, expression) =
      _parseExpression(retParser, Precedence.lowest);
  final stmt = ReturnStatement(expression);

  final newParser = eatLastSemicolon(exprParser);
  return (newParser, stmt);
}

(Parser parser, Expression expression) _parseExpression(
  Parser parser,
  Precedence precedence,
) {
  logger.detail('Parsing expression ${parser.currToken.value}');
  final prefix = _prefixParseFns[parser.currToken.type];
  if (prefix == null) {
    final errParser = eatLastSemicolon(parser);
    return (
      errParser.copyWith(
        errors: [
          ParserException(
            'No prefix parse function for ${parser.currToken.value}',
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
  logger.detail('_Parsing expression ${parser.currToken.value}');
  if (_peekTokenIs(parser, TokenType.semicolon) ||
      isCurrGtePeek(parser, precedence)) {
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
  return switch (parser.peekToken.type) {
    TokenType.ident => (
        advanceParser(parser),
        Identifier(parser.peekToken.value)
      ),
    _ => (
        parser.copyWith(
          errors: [
            ParserException(
              'Expected token to be ${TokenType.ident.name}, '
              'got ${parser.peekToken.type.name} instead',
              parser,
              [],
            )
          ],
        ),
        const NullExpression()
      ),
  };
}

(Parser parser, Expression expression) _parseExpressionIdentifier(
  Parser parser,
) {
  return switch (parser.currToken.type) {
    TokenType.ident => (parser, Identifier(parser.currToken.value)),
    _ => (
        parser.copyWith(
          errors: [
            ParserException(
              'Expected token to be ${TokenType.ident.name}, '
              'got ${parser.peekToken.type.name} instead',
              parser,
              [],
            )
          ],
        ),
        const NullExpression()
      ),
  };
}

Parser eatLastSemicolon(Parser parser) {
  return switch (parser.peekToken.type) {
    TokenType.semicolon => advanceParser(parser),
    _ => parser,
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

(Parser, Expression) _parseBoolean(Parser parser) {
  return (parser, BooleanLiteral(value: _currTokenIs(parser, TokenType.true_)));
}

(Parser, Expression) _parseGroupedExpression(Parser parser) {
  final advParser = advanceParser(parser);
  final (exprParser, expression) =
      _parseExpression(advParser, Precedence.lowest);
  final (newParser, ok) = expectPeek(exprParser, TokenType.rParen);
  if (!ok) {
    return (newParser, const NullExpression());
  }
  return (newParser, expression);
  // return (advanceParser(newParser), expression);
}

(Parser, Expression) _parseIfExpression(Parser parse) {
  logger.detail('Parsing if expression');
  final (expectParser, ok) = expectPeek(parse, TokenType.lParen);
  if (!ok) {
    return (expectParser, const NullExpression());
  }

  // move onto the condition
  final jumpLParendParser = advanceParser(expectParser);
  final (conditionPasrer, condition) =
      _parseExpression(jumpLParendParser, Precedence.lowest);

  final (jumpRParenParser, okEndCond) =
      expectPeek(conditionPasrer, TokenType.rParen);
  if (!okEndCond) {
    return (jumpRParenParser, const NullExpression());
  }

  final (jumpLSquirly, okCons) =
      expectPeek(jumpRParenParser, TokenType.lSquirly);
  if (!okCons) {
    return (jumpLSquirly, const NullExpression());
  }

  // gather the consequence
  final (consequenceParser, consequence) = _parseBlockStatement(jumpLSquirly);

  // gather the alternative, if any
  if (_peekTokenIs(consequenceParser, TokenType.else_)) {
    final elseParser = advanceParser(consequenceParser);
    final (jumpLBraceParser, okAlt) =
        expectPeek(elseParser, TokenType.lSquirly);
    if (!okAlt) {
      return (jumpLBraceParser, const NullExpression());
    }
    final (alternativeParser, alternative) =
        _parseBlockStatement(jumpLBraceParser);
    return (
      alternativeParser,
      IfExpression(
        condition: condition,
        consequence: consequence,
        alternative: alternative,
      )
    );
  } else {
    return (
      consequenceParser,
      IfExpression(condition: condition, consequence: consequence)
    );
  }
}

(Parser, BlockStatement) _parseBlockStatement(Parser parser) {
  logger.detail('Parsing block statement: ${parser.currToken.value}');
  final advParser = advanceParser(parser);
  final (blockParser, block) = __parseBlockStatement(advParser, []);

  return (blockParser, BlockStatement(block));
}

(Parser, List<Statement>) __parseBlockStatement(
  Parser parser,
  List<Statement> statements,
) {
  logger.detail('_Parsing block statement: ${parser.currToken.value}');

  switch (parser.currToken.type) {
    case TokenType.eof:
    case TokenType.rParen:
    case TokenType.rSquirly:
      return (parser, statements);
    // Some Token
    case _ when parser.currToken.type != TokenType.illegal:
      final (stmtParser, stmt) = _parseStatement(parser);
      return __parseBlockStatement(
        advanceParser(stmtParser),
        [...statements, stmt],
      );
    case _:
      return (
        parser.copyWith(
          errors: [
            ParserException(
              'unexpected eof',
              parser,
              statements,
            )
          ],
        ),
        statements,
      );
  }
}

(Parser, CallExpression) _parseCallExpression(
  Parser parser,
  Expression function,
) {
  final (argParser, args) = parseCallArguments(parser, []);

  if (argParser.currToken.type != TokenType.rParen) {
    return (
      argParser.copyWith(
        errors: [
          ParserException(
            'Expected next token to be ${TokenType.rParen}, '
            'got ${argParser.peekToken.type} instead',
            argParser,
            [],
            // line: argParser.peekToken.line,
            // column: argParser.peekToken.column,
          )
        ],
      ),
      const NullExpression() as CallExpression
    );
  }

  return (argParser, CallExpression(function, args));
}

(Parser, List<Expression>) parseCallArguments(
  Parser parser,
  List<Expression> args,
) {
  switch (parser.peekToken.type) {
    case TokenType.rParen:
      return (advanceParser(parser), args);
    case TokenType.eof:
      return (
        parser.copyWith(
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
        args,
      );
    case TokenType.comma:
      return parseCallArguments(advanceParser(parser), args);
    case _:
      final (newParser, arg) =
          _parseExpression(advanceParser(parser), Precedence.lowest);
      return parseCallArguments(newParser, [...args, arg]);
  }
}

(Parser, StringLiteral) _parseStringLiteral(Parser parser) {
  return (parser, StringLiteral(parser.currToken.value));
}

(Parser, FunctionLiteral) _parseFunctionLiteral(Parser parser) {
  final (jumpLParenParser, ok) = expectPeek(parser, TokenType.lParen);
  if (!ok) {
    return (jumpLParenParser, const NullExpression() as FunctionLiteral);
  }

  final (paramsParser, parameters) = _parseFnParameters(jumpLParenParser, []);

  final (jumpRParenParser, okEndParams) =
      expectPeek(paramsParser, TokenType.rParen);
  if (!okEndParams) {
    return (jumpRParenParser, const NullExpression() as FunctionLiteral);
  }

  final (jumpLBraceParser, okCons) =
      expectPeek(jumpRParenParser, TokenType.lSquirly);
  if (!okCons) {
    return (jumpLBraceParser, const NullExpression() as FunctionLiteral);
  }

  final (bodyParser, body) = _parseBlockStatement(jumpLBraceParser);

  return (
    bodyParser,
    FunctionLiteral(parameters: parameters, body: body),
  );
}

/// The initial call to [_parseFnParameters] should be made with an empty list
/// of parameters. The [parser] should be at the opening parenthesis of the fn
(Parser, List<Identifier>) _parseFnParameters(
  Parser parser,
  List<Identifier> parameters,
) {
  return switch (parser.peekToken.type) {
    TokenType.rParen => (parser, parameters),
    TokenType.ident => _parseFnParameters(
        advanceParser(parser),
        [...parameters, Identifier(parser.peekToken.value)],
      ),
    TokenType.comma => _parseFnParameters(
        advanceParser(parser),
        parameters,
      ),
    TokenType.eof => (
        parser.copyWith(
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
        parameters,
      ),
    _ => (
        parser.copyWith(
          errors: [
            ParserException(
              'Expected next token to be ${TokenType.ident}, '
              'got ${parser.peekToken.type} instead',
              parser,
              [],
              // line: parser.peekToken.line,
              // column: parser.peekToken.column,
            )
          ],
        ),
        parameters,
      ),
  };
}

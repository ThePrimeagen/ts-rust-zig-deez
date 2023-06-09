// import 'package:mason_logger/mason_logger.dart';
// import 'package:meta/meta.dart';
// import 'package:monkeydart/monkeydart.dart';

// final logger = Logger(level: Level.debug);

// // unary operators
// final _prefixParseFns = <TokenType,
//     Result<(Parser, Expression), ParserException> Function(Parser parser)>{
//   TokenType.ident: _parseIdentifier,
//   // TokenType.int: () => IntegerLiteral(),
//   // TokenType.bang: () => PrefixExpression(),
//   // TokenType.dash: () => PrefixExpression(),
//   // TokenType.true: () => Boolean(),
//   // TokenType.false: () => Boolean(),
//   // TokenType.lparen: () => GroupedExpression(),
// };

// // binary operators
// final _infixParseFns = <TokenType,
//     Result<(Parser, Expression), ParserException> Function(Expression)>{};

// @immutable
// class Parser {
//   factory Parser(Iterable<Token> tokens, {int tokenIndex = 0}) {
//     if (tokens.isEmpty || tokenIndex >= tokens.length) {
//       return const NullParser();
//     } else {
//       final currToken = tokenIndex < tokens.length
//           ? tokens.elementAt(tokenIndex)
//           : const Token.illegal();
//       final peekToken = tokenIndex + 1 < tokens.length
//           ? tokens.elementAt(tokenIndex + 1)
//           : const Token.illegal();
//       return Parser._(
//         tokens,
//         tokenIndex,
//         currToken,
//         peekToken,
//       );
//     }
//   }

//   factory Parser.fromLexer(Lexer lexer) {
//     return Parser(
//       tokenGenerator(lexer.source),
//     );
//   }

//   factory Parser.fromSource(String source) {
//     return Parser(
//       tokenGenerator(source),
//     );
//   }

//   const Parser._(
//     this.tokens,
//     this.tokenIndex,
//     this.currToken,
//     this.peekToken,
//     this.errors,
//   );

//   final Iterable<Token> tokens;
//   final int tokenIndex;
//   final Token currToken;
//   final Token peekToken;
//   final List<ParserException> errors;

//   Parser copyWith({
//     Iterable<Token>? tokens,
//     int? tokenIndex,
//     Token? currToken,
//     Token? peekToken,
//     List<ParserException>? errors,
//   }) {
//     return Parser._(
//       tokens ?? this.tokens,
//       tokenIndex ?? this.tokenIndex,
//       currToken ?? this.currToken,
//       peekToken ?? this.peekToken,
//       errors ?? this.errors,
//     );
//   }

//   @override
//   String toString() {
//     final retVal = StringBuffer('Parser{')
//     ..write('Tokens: ${tokenIndex + 1} of ${tokens.length}, ')
//     ..write('\n\tcurrToken: $currToken, ')
//     ..write('\n\tpeekToken: $peekToken');
//     if (errors.isNotEmpty) {
//       retVal.write('\n\tErrors: $errors');
//     }
//     retVal.write('}');
//     return retVal.toString();
//   }
// }

// class NullParser extends Parser {
//   const NullParser()
//       : super._(const [], 0, const Token.illegal(), const Token.illegal());
// }

// Result<Parser, ParserException> advanceParser(Parser parser) {
//   if (parser.tokenIndex + 1 >= parser.tokens.length) {
//     return Err(
//       ParserException(
//         'Cannot advance parser past the end of the token stream',
//         parser,
//         [],
//       ),
//     );
//   }
//   final nextParser = parser.copyWith(
//     tokenIndex: parser.tokenIndex + 1,
//     currToken: parser.peekToken,
//     peekToken: parser.tokenIndex + 2 >= parser.tokens.length
//         ? const Token.illegal()
//         : parser.tokens.elementAt(parser.tokenIndex + 2),
//   );
//   return Ok(nextParser);
// }

// Result<(Parser, Token), ParserException> nextParserToken(Parser parser) {
//   final result = advanceParser(parser);

//   final retVal = switch (result) {
//     Ok<Parser, ParserException>(value: final outcome) => (
//         outcome,
//         outcome.currToken
//       ),
//     Err<Parser, ParserException>(error: final error) => error,
//   };
//   if (retVal is ParserException) {
//     return Err(retVal);
//   }
//   return Ok(retVal as (Parser, Token));
// }

// Result<Program, ParserException> parse(Parser parser) {
//   final parserStatements = _parse(parser, []);
//   final retVal = switch (parserStatements) {
//     Ok<(Parser, List<Statement>), ParserException>(value: final outcome) =>
//       Ok(Program(outcome.$2)),
//     Err<(Parser, List<Statement>), ParserException>(error: final err) =>
//       Err<Program, ParserException>(err)
//   };

//   return retVal;
// }

// Result<(Parser, List<Statement>), ParserException> _parse(
//   Parser parser,
//   List<Statement> statements,
// ) {
//   switch (parser.currToken.type) {
//     case TokenType.eof:
//       return Ok((parser, statements));
//     case _:
//       final result = _parseStatement(parser);
//       final retVal = switch (result) {
//         Ok<(Parser, Statement), ParserException>(value: final outcome) =>
//           _parse(
//             // may need to advanceParser here
//             outcome.$1,
//             statements..add(outcome.$2),
//           ),
//         // TODO how do I load the errors?
//         Err<(Parser, Statement), ParserException>(error: final error) =>
//           Err<(Parser, List<Statement>), ParserException>(error),
//       };
//       return retVal as Result<(Parser, List<Statement>), ParserException>;
//   }
// }

// Result<(Parser, Statement), ParserException> _parseStatement(Parser parser) {
//   switch (parser.currToken.type) {
//     case TokenType.let:
//       return _parseLetStatement(parser);
//     case TokenType.return_:
//       return _parseReturnStatement(parser);

//     case _:
//       return _parseExpressionStatement(parser);
//   }
// }

// Result<(Parser, Statement), ParserException> _parseLetStatement(Parser parser) {
//   // let five = 5;
//   // I should be sitting on the Let token in order to ender this fn

//   // Is the next token an identifier?
//   final idPeekResult = _expectPeek(parser, TokenType.ident);
//   if (idPeekResult is Err<Parser, ParserException>) {
//     return idPeekResult as Result<(Parser, Statement), ParserException>;
//   }
//   final idParser = (idPeekResult as Ok<Parser, ParserException>).value;

//   // Parser is now sitting on the identifier
//   final idResult = _parseIdentifier(idParser);
//   if (idResult is Err<Parser, ParserException>) {
//     return idResult as Result<(Parser, Statement), ParserException>;
//   }
//   final (_, identifier) =
//       (idResult as Ok<(Parser, Identifier), ParserException>).value;

//   final assPeekResult = _expectPeek(idParser, TokenType.assign);
//   if (assPeekResult is Err<Parser, ParserException>) {
//     return Err(assPeekResult.error);
//   }
//   // // Parser is now sitting on the assign token and we have the identifier
//   final exprParserResult = advanceParser(idParser);
//   if (exprParserResult is Err<Parser, ParserException>) {
//     return exprParserResult as Result<(Parser, Statement), ParserException>;
//   }
//   final exprParser = (exprParserResult as Ok<Parser, ParserException>).value;

//   // Parser is now sitting on the expression token
//   final value = Expression(exprParser.peekToken);

//   final lastParserResult = _eatNextSemicolon(exprParser);
//   if (lastParserResult is Err<Parser, ParserException>) {
//     return lastParserResult as Result<(Parser, Statement), ParserException>;
//   }

//   return Ok(
//     (
//       (lastParserResult as Ok<Parser, ParserException>).value,
//       LetStatement(identifier, value)
//     ),
//   );
// }

// Result<(Parser, Statement), ParserException> _parseReturnStatement(
//   Parser parser,
// ) {
//   // I don't care about the current token; it is a return token
//   // advance the parser
//   // return the Parser and the ReturnStatement with an expression of
//   // the next token
//   final nextParser = switch (advanceParser(parser)) {
//     Ok<Parser, ParserException>(value: final nextParser) => nextParser,
//     Err<Parser, ParserException>(error: final err) => err,
//   } as Parser;

//   final value = Expression(nextParser.currToken);

//   final lastResult = _eatNextSemicolon(nextParser);
//   final lastParser = switch (lastResult) {
//     Ok<Parser, ParserException>(value: final nextParser) => nextParser,
//     Err<Parser, ParserException>(error: final err) => err,
//   } as Parser;

//   return Ok(
//     (lastParser, ReturnStatement(value)),
//   );
// }

// Result<(Parser, Identifier), ParserException> _parseIdentifier(Parser parser) {
//   if (!_currTokenIs(parser, TokenType.ident)) {
//     return Err(
//       ParserException(
//         'expected next token to be ${TokenType.ident}, '
//         'got ${parser.currToken.type} instead',
//         parser,
//         [],
//       ),
//     );
//   }
//   return Ok(
//     (parser, Identifier(parser.currToken.value)),
//   );
// }

// Result<(Parser, ExpressionStatement), ParserException>
//     _parseExpressionStatement(
//   Parser parser,
// ) {
//   final parseResult = _parseExpression(parser, Precedence.lowest);
//   if (parseResult is Err<Parser, ParserException>) {
//     return Err<(Parser, ExpressionStatement), ParserException>(
//       (parseResult as Err<Parser, ParserException>).error,
//     );
//   }
//   final (newParser, expression) = switch (parseResult) {
//     Ok<(Parser, Expression), ParserException>(value: final outcome) => (
//         outcome.$1,
//         outcome.$2
//       ),
//     Err<(Parser, Expression), ParserException>(error: final err) => err,
//   } as (Parser, Expression);

//   // final newParser = switch (_eatNextSemicolon(parser)) {
//   //   Ok<Parser, ParserException>(value: final nextParser) => nextParser,
//   //   Err<Parser, ParserException>(error: final err) => err,
//   // } as Parser;

//   return Ok<(Parser, ExpressionStatement), ParserException>(
//     (newParser, ExpressionStatement(expression)),
//   );
// }

// Result<(Parser, Expression), ParserException> _parseExpression(
//   Parser parser,
//   Precedence precedence,
// ) {
//   final prefix = _prefixParseFns[parser.currToken.type];
//   if (prefix == null) {
//     return Err(
//       ParserException(
//         'no prefix parse function for ${parser.currToken.type} found',
//         parser,
//         [],
//       ),
//     );
//   }
//   final leftResult = prefix(parser);
//   if (leftResult is Err<Parser, ParserException>) {
//     return leftResult;
//   }
//   final (leftParser, left) = switch (leftResult) {
//     Ok<(Parser, Expression), ParserException>(value: final outcome) => (
//         outcome.$1,
//         outcome.$2
//       ),
//     Err<(Parser, Expression), ParserException>(error: final err) => err,
//   } as (Parser, Expression);

//   final nextParserResult = advanceParser(leftParser);
//   if (nextParserResult is Err<Parser, ParserException>) {
//     return Err<(Parser, Expression), ParserException>(nextParserResult.error);
//   }
//   final nextParser = (nextParserResult as Ok<Parser, ParserException>).value;

//   return Ok<(Parser, Expression), ParserException>(
//     (nextParser, left),
//   );
// }

// Result<Parser, ParserException> _eatNextSemicolon(Parser parser) {
//   switch (parser.currToken.type) {
//     case TokenType.semicolon:
//       // eat 1 more token
//       final lastParser = switch (advanceParser(parser)) {
//         Ok<Parser, ParserException>(value: final nextParser) => nextParser,
//         Err<Parser, ParserException>(error: _) => NullParser,
//       } as Parser;
//       return Ok(lastParser);
//     case TokenType.eof:
//       return Err(
//         ParserException(
//           'Ran out of tokens before encountering a semicolon',
//           parser,
//           [],
//         ),
//       );
//     case TokenType.illegal:
//       return Err(
//         ParserException(
//           'Encountered an illegal token before encountering a semicolon',
//           parser,
//           [],
//         ),
//       );
//     case _:
//       final result = advanceParser(parser);
//       final retVal = switch (result) {
//         Ok<Parser, ParserException>(value: final nextParser) =>
//           _eatNextSemicolon(nextParser),
//         Err<Parser, ParserException>(error: final err) => err,
//       };
//       return retVal as Result<Parser, ParserException>;
//   }
// }

// Result<Parser, ParserException> _expectPeek(Parser parser, TokenType type) {
//   // Test for expected token type
//   // If it is the expected token type, advance the parser
//   // If it is not the expected token type, return an error

//   // test affirmative first as most likely outcome
//   if (_peekTokenIs(parser, type)) {
//     return switch (advanceParser(parser)) {
//       Ok<Parser, ParserException>(value: final outcome) => Ok(outcome),
//       // forward the advance error: RARE AF
//       Err<Parser, ParserException>(error: final err) => err,
//     } as Result<Parser, ParserException>;
//   } else {
//     return Err(
//       ParserException(
//         'expected next token to be $type, got ${parser.peekToken.type} instead',
//         parser,
//         [],
//       ),
//     );
//   }
// }

// bool _currTokenIs(Parser parser, TokenType type) {
//   return parser.currToken.type == type;
// }

// bool _peekTokenIs(Parser parser, TokenType type) {
//   return parser.peekToken.type == type;
// }

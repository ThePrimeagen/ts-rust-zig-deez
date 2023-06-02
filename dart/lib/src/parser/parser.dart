import 'dart:io';

import 'package:dart_deez/src/lexer/lexer.dart';
import 'ast.dart';

export 'ast.dart';

String red(String s) => "\x1b[31m$s\x1b[0m";

Never parseError(String message) {
  print(message);
  print(StackTrace.current);
  exit(1);
}

class Parser {
  final Lexer lexer;
  final errors = <String>[];
  // late implies that the variable will be initialized but not directly in the constructor.
  late Token _currentToken = const Token.eof();
  late Token _peekToken = const Token.eof();

  Parser(this.lexer) {
    _nextToken();
    _nextToken();
  }

  Program parseProgram() {
    final statements = <Statement>[];

    while (_currentToken.type != TokenType.eof) {
      final statement = _parseStatement();
      if (statement != null) {
        statements.add(statement);
      }
      _nextToken();
    }

    return Program(statements);
  }

  Statement? _parseStatement() {
    return switch (_currentToken.type) {
      TokenType.let => _parseLetStatement(),
      TokenType.return_ => _parseReturnStatement(),
      _ => null,
    };
  }

  Statement? _parseLetStatement() {
    final token = _currentToken;

    if (!_expectPeek(TokenType.ident)) {
      return null;
    }

    final name = Identifier(_currentToken, _currentToken.literal);

    if (!_expectPeek(TokenType.assign)) {
      return null;
    }

    while (!_currentTokenIs(TokenType.semicolon)) {
      _nextToken();
    }

    return LetStatement(
      token,
      name,
      Identifier(_currentToken, _currentToken.literal),
    );
  }

  Statement? _parseReturnStatement() {
    final token = _currentToken;

    _nextToken();

    while (!_currentTokenIs(TokenType.semicolon)) {
      _nextToken();
    }

    return ReturnStatement(
      token,
      Identifier(_currentToken, _currentToken.literal),
    );
  }

  bool _currentTokenIs(TokenType type) {
    return _currentToken.type == type;
  }

  bool _peekTokenIs(TokenType type) {
    return _peekToken.type == type;
  }

  void _nextToken() {
    _currentToken = _peekToken;
    _peekToken = lexer.nextToken();
  }

  bool _expectPeek(TokenType type) {
    if (_peekToken.type == type) {
      _nextToken();
      return true;
    } else {
      _peekError(type);
      return false;
    }
  }

  void _peekError(TokenType type) {
    errors.add(
      red("Expected next token to be ${type.name}, got ${_peekToken.type.name}"),
    );
  }

  bool checkParserErrors() {
    if (errors.isEmpty) {
      return false;
    }
    print(errors.join("\n"));
    return true;
  }
}

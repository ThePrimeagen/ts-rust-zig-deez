import 'package:dart_deez/dart_deez.dart';

/*  ====================================================================  
  Precedence artifacts
  ====================================================================  */
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

/*  ====================================================================  
  Tree Node artifacts
  ====================================================================  */

/// Basic element of a program
class Node {
  Node(this.token);
  final Token token;

  String tokenLiteral() {
    return token.literal;
  }

  @override
  String toString() {
    return '${token.type.name} ${token.literal}';
  }
}

/// Statements do not return values
class Statement extends Node {
  Statement(super.token);

  @override
  String toString() {
    return token.literal;
  }
}

/// Expressions return values
class Expression extends Node {
  Expression(super.token);

  @override
  String toString() {
    return token.literal;
  }
}

class Program extends Node {
  Program() : super(const Token.eof());
  final List<Statement> statements = [];
  @override
  String tokenLiteral() {
    if (statements.isNotEmpty) {
      return statements.first.token.literal;
    }
    return '';
  }

  @override
  String toString() {
    return statements.join();
  }
}

class LetStatement extends Statement {
  LetStatement(this.name, this.value) : super(const Token.let());
  final Identifier name;
  final Expression value;
  @override
  String toString() {
    return '${token.type.name} ${name.value} = ${value.token.literal};';
  }
}

class ReturnStatement extends Statement {
  ReturnStatement(this.returnValue) : super(const Token.return_());
  final Expression returnValue;
  @override
  String toString() {
    return '${token.type.name.replaceAll('_', '')} ${returnValue.token.literal};';
  }
}

class Identifier extends Expression {
  Identifier(this.value) : super(Token.ident(value));
  final String value;
  @override
  String toString() {
    return value;
  }
}

class ExpressionStatement extends Statement {
  ExpressionStatement(this.expression) : super(expression.token);
  final Expression expression;

  @override
  String toString() {
    return expression.toString();
  }
}

class IntegerLiteral extends Expression {
  IntegerLiteral(this.value) : super(Token.int(value.toString()));
  final int value;

  @override
  String toString() {
    return token.literal;
  }
}

class PrefixExpression extends Expression {
  PrefixExpression(super.token, this.operator, this.right);
  final String operator;
  final Expression right;

  @override
  String toString() {
    return '($operator$right)';
  }
}

class InfixExpression extends Expression {
  InfixExpression(
    super.token,
    this.left,
    this.operator,
    this.right,
  );
  final Expression left;
  final String operator;
  final Expression right;

  @override
  String tokenLiteral() {
    return token.literal;
  }

  @override
  String toString() {
    return '($left $operator $right)';
  }
}

/*  ====================================================================  
  The Parser
  ====================================================================  */

class Parser {
  Parser(this.lexer) {
    // grab the first token from the lexer
    _curToken = lexer.nextToken();
    // grab the second token from the lexer
    _peekToken = lexer.nextToken();
    initInfixAndPrefixFns();
  }

/*  ====================================================================  
  Local vars
  ====================================================================  */
  final Tokenizer lexer;
  late Token _curToken;
  late Token _peekToken;
  final _errors = <String>[];

  List<String> get errors => _errors;

/*  ====================================================================  
  Token walking functions
  ====================================================================  */

  void _nextToken() {
    _curToken = _peekToken;
    _peekToken = lexer.nextToken();
  }

  bool _curTokenIs(TokenType type) {
    return _curToken.type == type;
  }

  bool _peekTokenIs(TokenType type) {
    return _peekToken.type == type;
  }

  bool _expectPeek(TokenType type) {
    if (_peekTokenIs(type)) {
      _nextToken();
      return true;
    } else {
      _peekError(type);
      return false;
    }
  }

  void _peekError(TokenType type) {
    _errors.add(
      'expected next token to be $type, got ${_peekToken.type} instead',
    );
  }

  Map<TokenType, Precedence> precedences() {
    return {
      TokenType.eq: Precedence.equals,
      TokenType.neq: Precedence.equals,
      TokenType.lt: Precedence.lessGreater,
      TokenType.gt: Precedence.lessGreater,
      TokenType.plus: Precedence.sum,
      TokenType.dash: Precedence.sum,
      TokenType.slash: Precedence.product,
      TokenType.asterisk: Precedence.product,
    };
  }

/*  ====================================================================  
  Prefix(unary) and Infix(binary) functions
      All complex statements are trees of binary and unary expressions
  ====================================================================  */
  void registerPrefix(TokenType type, Expression Function() fn) {
    _prefixParseFns[type] = fn;
  }

  void registerInfix(TokenType type, Expression Function(Expression) fn) {
    _infixParseFns[type] = fn;
  }

  void _noPrefixParseFnError(Token t) {
    _errors.add(
        'no prefix parse functions found for ${t.type.name}:(${t.literal})');
  }

  void _noInfixParseFnError(Token t) {
    _errors.add(
        'no infix parse functions found for ${t.type.name}:(${t.literal})');
  }

  void initInfixAndPrefixFns() {
    registerPrefix(TokenType.ident, _parseIdentifier);
    registerPrefix(TokenType.int, _parseIntegerLiteral);
    registerPrefix(TokenType.bang, _parsePrefixExpression);
    registerPrefix(TokenType.dash, _parsePrefixExpression);

    registerInfix(TokenType.gt, _parseInfixExpression);
    registerInfix(TokenType.lt, _parseInfixExpression);
    registerInfix(TokenType.plus, _parseInfixExpression);
    registerInfix(TokenType.dash, _parseInfixExpression);
    registerInfix(TokenType.slash, _parseInfixExpression);
    registerInfix(TokenType.asterisk, _parseInfixExpression);
    registerInfix(TokenType.eq, _parseInfixExpression);
    registerInfix(TokenType.neq, _parseInfixExpression);
  }

  final _prefixParseFns = <TokenType, Expression Function()>{};

  final _infixParseFns = <TokenType, Expression Function(Expression)>{};

  Program parseProgram() {
    final program = Program();
    while (!_curTokenIs(TokenType.eof)) {
      final stmt = _parseStatement();
      if (stmt != null) {
        program.statements.add(stmt);
      }
      _nextToken();
    }
    return program;
  }

  Statement? _parseStatement() {
    switch (_curToken.type) {
      case TokenType.let:
        return _parseLetStatement();
      case TokenType.return_:
        return _parseReturnStatement();
      // ignore: no_default_cases
      default:
        return _parseExpressionStatement();
    }
  }

  Statement? _parseReturnStatement() {
    _nextToken();

    final returnStatement = ReturnStatement(Expression(_curToken));
    if (_peekToken.type != TokenType.semicolon) {
      // eat the end of line token
      _nextToken();
      return null;
    } else {
      _nextToken();
    }
    return returnStatement;
  }

  Statement? _parseLetStatement() {
    /// We must have a let token to be in the function
    /// our next token must be an identifier
    if (!_expectPeek(TokenType.ident)) {
      // _expectPeek advanced the token and registered an error
      // _parseError('missing expected identifier token');
      while (_peekToken.type != TokenType.semicolon) {
        // eat the end of line token
        _nextToken();
      }
      _nextToken();
      return null;
    }

    // I feel good that I have an identifier / expectPeek advanced the token
    final name = Identifier(_curToken.literal);

    /// We now have Let identifier and expect an assignment token
    if (!_expectPeek(TokenType.equal)) {
      // _expectPeek advanced the token and registered an error
      // _parseError('missing expected equal token');
      while (_peekToken.type != TokenType.semicolon) {
        // eat the end of line token
        _nextToken();
      }
      _nextToken();
      return null;
    }

    _nextToken();

    /// We should nw have Let identifier = _____
    /// and we expect an expression as a value
    final value = Expression(_curToken);

    final letStatement = LetStatement(name, value);

    if (_peekToken.type != TokenType.semicolon) {
      // eat the end of line token
      _nextToken();
      return null;
    } else {
      _nextToken();
    }

    return letStatement;
  }

  ExpressionStatement? _parseExpressionStatement() {
    final expression = _parseExpression(Precedence.lowest);
    if (expression == null) {
      return null;
    }
    final stmt = ExpressionStatement(expression);
    if (_peekTokenIs(TokenType.semicolon)) {
      _nextToken();
    }
    return stmt;
  }

  Expression? _parseExpression(Precedence precedence) {
    final prefix = _prefixParseFns[_curToken.type];
    if (prefix == null) {
      // no prefix parse function found
      _noPrefixParseFnError(_curToken);
      return null;
    }
    var leftExp = prefix();

    while (!_peekTokenIs(TokenType.semicolon) &&
        precedence.index < _peekPrecedence().index) {
      final infix = _infixParseFns[_peekToken.type];
      if (infix == null) {
        // no infix parse function found
        _noInfixParseFnError(_peekToken);
        return leftExp;
      }
      _nextToken();
      leftExp = infix(leftExp);
    }

    return leftExp;
  }

  Expression _parseInfixExpression(Expression left) {
    final curToken = _curToken;
    final operator = curToken.literal;
    final precedence = _curPrecedence();
    _nextToken();
    final right = _parseExpression(precedence);
    final expression = InfixExpression(
      curToken,
      left,
      operator,
      right ?? Expression(curToken),
    );
    return expression;
  }

  Expression _parsePrefixExpression() {
    final curToken = _curToken;
    _nextToken();
    final right = _parseExpression(Precedence.prefix);
    final expression = PrefixExpression(
      curToken,
      curToken.literal,
      right ?? Expression(curToken),
    );
    return expression;
  }

  Expression _parseIdentifier() {
    return Identifier(_curToken.literal);
  }

  Expression _parseIntegerLiteral() {
    return IntegerLiteral(int.parse(_curToken.literal));
  }

  Precedence _curPrecedence() {
    final precedence = precedences()[_curToken.type];
    if (precedence != null) {
      return precedence;
    }
    return Precedence.lowest;
  }

  Precedence _peekPrecedence() {
    final precedence = precedences()[_peekToken.type];
    if (precedence != null) {
      return precedence;
    }
    return Precedence.lowest;
  }

  void _parseError(String msg) {
    _errors.add('Parsing error: $msg');
  }
}

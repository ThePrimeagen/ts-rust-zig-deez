export const TokenType = {
  Equal: 'EQUAL',
  NotEqual: 'NOT_EQUAL',
  GreaterEqual: 'GREATER_THAN',
  Greater: 'GREATER',
  LessEqual: 'LESS_THAN',
  Less: 'LESS',
  Bang: 'BANG',
  Assign: 'ASSIGN',
  Semicolon: 'SEMICOLON',
  Comma: 'COMMA',
  LParen: 'L_PAREN',
  RParen: 'R_PARENT',
  LSquirly: 'L_SQUIRLY',
  RSquirly: 'R_SQUIRLY',
  Plus: 'PLUS',
  Minus: 'MINUS',
  Asterisk: 'ASTERISK',
  ForwardSlash: 'FORWARD_SLASH',
  Let: 'LET',
  Function: 'FUNCTION',
  Return: 'RETURN',
  If: 'IF',
  Else: 'ELSE',
  True: 'TRUE',
  False: 'FALSE',
  Int: 'INT',
  Ident: 'IDENT',
  Eof: 'EOF',
  Illegal: 'ILLEGAL',
} as const;

type TokenItem = (typeof TokenType)[keyof typeof TokenType];

export type Token = {
  type: TokenItem;
  literal: string;
};

export const createToken = (type: TokenItem, literal: string): Token => {
  return { type, literal };
};

export const Keywords = {
  let: createToken(TokenType.Let, 'let'),
  fn: createToken(TokenType.Function, 'fn'),
  return: createToken(TokenType.Return, 'return'),
  true: createToken(TokenType.True, 'true'),
  false: createToken(TokenType.False, 'false'),
  if: createToken(TokenType.If, 'if'),
  else: createToken(TokenType.Else, 'else'),
} as const;

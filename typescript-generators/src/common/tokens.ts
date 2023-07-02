export type Token = SingleToken | AmbiguousToken | ReservedToken | SpecialToken;

type SingleTokenDict = typeof SINGLE_TOKEN;
export type SingleTokenType = keyof SingleTokenDict;
export type SingleToken = SingleTokenDict[SingleTokenType];
export const SINGLE_TOKEN = {
  123: { type: '{', literal: '{' },
  125: { type: '}', literal: '}' },
  40: { type: '(', literal: '(' },
  41: { type: ')', literal: ')' },
  60: { type: '<', literal: '<' },
  62: { type: '>', literal: '>' },
  45: { type: '-', literal: '-' },
  43: { type: '+', literal: '+' },
  44: { type: ',', literal: ',' },
  59: { type: ';', literal: ';' },
  47: { type: '/', literal: '/' },
  42: { type: '*', literal: '*' },
} as const;

type ReservedTokenDict = typeof RESERVED_TOKEN;
export type ReservedTokenType = keyof ReservedTokenDict;
export type ReservedToken = ReservedTokenDict[ReservedTokenType];
export const RESERVED_TOKEN = {
  true: { type: 'true', literal: 'true' },
  false: { type: 'false', literal: 'false' },
  if: { type: 'if', literal: 'if' },
  else: { type: 'else', literal: 'else' },
  let: { type: 'let', literal: 'let' },
  fn: { type: 'fn', literal: 'fn' },
  return: { type: 'return', literal: 'return' },
} as const;

export type AmbiguousToken =
  | typeof TOKEN_ASSIGNMENT
  | typeof TOKEN_BANG
  | typeof TOKEN_INEQUALITY
  | typeof TOKEN_EQUALITY;
export const TOKEN_ASSIGNMENT = { type: '=', literal: '=' } as const;
export const TOKEN_BANG = { type: '!', literal: '!' } as const;
export const TOKEN_INEQUALITY = { type: '!=', literal: '!=' } as const;
export const TOKEN_EQUALITY = { type: '==', literal: '==' } as const;

export type SpecialTokenType =
  | typeof TOKEN_TYPE_INT
  | typeof TOKEN_TYPE_ID
  | typeof TOKEN_TYPE_ILLEGAL;
export type SpecialToken = { type: SpecialTokenType; literal: string };
export const TOKEN_TYPE_INT = '<int>';
export const TOKEN_TYPE_ID = '<identifier>';
export const TOKEN_TYPE_ILLEGAL = '<illegal>';

export type EofToken = typeof TOKEN_EOF;
export const TOKEN_EOF = { type: '<eof>', literal: '\0' } as const;

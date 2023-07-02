import { Readable } from 'node:stream';
import { expect, test } from '@jest/globals';

import {
  RESERVED_TOKEN,
  SINGLE_TOKEN,
  TOKEN_ASSIGNMENT,
  TOKEN_BANG,
  TOKEN_EQUALITY,
  TOKEN_INEQUALITY,
  TOKEN_TYPE_ID,
  TOKEN_TYPE_INT,
  Token,
} from '../../common';
import { zipAsync } from '../../utils';
import { createChunkLexer } from '../createChunkLexer';

test('test stream input', async () => {
  const stream = Readable.from([
    Buffer.from('=+(){},;'),
    Buffer.from('123\nar;'),
  ]);

  const lexer = createChunkLexer(stream);

  const testTokens: Token[] = [
    TOKEN_ASSIGNMENT,
    SINGLE_TOKEN[43],
    SINGLE_TOKEN[40],
    SINGLE_TOKEN[41],
    SINGLE_TOKEN[123],
    SINGLE_TOKEN[125],
    SINGLE_TOKEN[44],
    SINGLE_TOKEN[59],
    { type: TOKEN_TYPE_INT, literal: '123' },
    { type: TOKEN_TYPE_ID, literal: 'ar' },
    SINGLE_TOKEN[59],
  ];

  for await (const [lexResult, testResult] of zipAsync(lexer, testTokens)) {
    expect(lexResult.done).toEqual(testResult.done);
    if (lexResult.done && testResult.done) continue;
    expect(lexResult.value).toEqual(testResult.value);
  }
});

test('test bigger stream input', async () => {
  const stream = Readable.from(Buffer.from(`
    let five = 5;
    let ten = 10;
    let add = fn(x, y) {
      x + y;
    };
    let result = add(five, ten);
    !-/*5;
    5 < 10 > 5;
    if (5 < 10) {
      return true;
    } else {
      return false;
    }
    10 == 10;
    10 != 9;
  `));

  const lexer = createChunkLexer(stream);

  const testTokens: Token[] = [
    RESERVED_TOKEN.let,
    { type: TOKEN_TYPE_ID, literal: 'five' },
    TOKEN_ASSIGNMENT,
    { type: TOKEN_TYPE_INT, literal: '5' },
    SINGLE_TOKEN[59],
    RESERVED_TOKEN.let,
    { type: TOKEN_TYPE_ID, literal: 'ten' },
    TOKEN_ASSIGNMENT,
    { type: TOKEN_TYPE_INT, literal: '10' },
    SINGLE_TOKEN[59],
    RESERVED_TOKEN.let,
    { type: TOKEN_TYPE_ID, literal: 'add' },
    TOKEN_ASSIGNMENT,
    RESERVED_TOKEN.fn,
    SINGLE_TOKEN[40],
    { type: TOKEN_TYPE_ID, literal: 'x' },
    SINGLE_TOKEN[40],
    { type: TOKEN_TYPE_ID, literal: 'y' },
    SINGLE_TOKEN[41],
    SINGLE_TOKEN[123],
    { type: TOKEN_TYPE_ID, literal: 'x' },
    SINGLE_TOKEN[43],
    { type: TOKEN_TYPE_ID, literal: 'y' },
    SINGLE_TOKEN[59],
    SINGLE_TOKEN[125],
    SINGLE_TOKEN[59],
    RESERVED_TOKEN.let,
    { type: TOKEN_TYPE_ID, literal: 'result' },
    TOKEN_ASSIGNMENT,
    { type: TOKEN_TYPE_ID, literal: 'add' },
    SINGLE_TOKEN[40],
    { type: TOKEN_TYPE_ID, literal: 'five' },
    SINGLE_TOKEN[40],
    { type: TOKEN_TYPE_ID, literal: 'ten' },
    SINGLE_TOKEN[41],
    SINGLE_TOKEN[59],
    TOKEN_BANG,
    SINGLE_TOKEN[45],
    SINGLE_TOKEN[47],
    SINGLE_TOKEN[42],
    { type: TOKEN_TYPE_INT, literal: '5' },
    SINGLE_TOKEN[59],
    { type: TOKEN_TYPE_INT, literal: '5' },
    SINGLE_TOKEN[60],
    { type: TOKEN_TYPE_INT, literal: '10' },
    SINGLE_TOKEN[62],
    { type: TOKEN_TYPE_INT, literal: '5' },
    SINGLE_TOKEN[59],
    RESERVED_TOKEN.if,
    SINGLE_TOKEN[40],
    { type: TOKEN_TYPE_INT, literal: '5' },
    SINGLE_TOKEN[60],
    { type: TOKEN_TYPE_INT, literal: '10' },
    SINGLE_TOKEN[41],
    SINGLE_TOKEN[123],
    RESERVED_TOKEN.return,
    RESERVED_TOKEN.true,
    SINGLE_TOKEN[59],
    SINGLE_TOKEN[125],
    RESERVED_TOKEN.else,
    SINGLE_TOKEN[123],
    RESERVED_TOKEN.return,
    RESERVED_TOKEN.false,
    SINGLE_TOKEN[59],
    SINGLE_TOKEN[125],
    { type: TOKEN_TYPE_INT, literal: '10' },
    TOKEN_EQUALITY,
    { type: TOKEN_TYPE_INT, literal: '10' },
    SINGLE_TOKEN[59],
    { type: TOKEN_TYPE_INT, literal: '10' },
    TOKEN_INEQUALITY,
    { type: TOKEN_TYPE_INT, literal: '9' },
    SINGLE_TOKEN[59],
  ];

  for await (const [lexResult, testResult] of zipAsync(lexer, testTokens)) {
    expect(lexResult.done).toEqual(testResult.done);
    if (lexResult.done && testResult.done) continue;
    expect(lexResult.value).toEqual(testResult.value);
  }
});

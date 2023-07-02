import {
  TOKEN_ASSIGNMENT,
  TOKEN_BANG,
  CODE_BANG,
  CODE_EQUAL,
  CODE_NEWLINE,
  CODE_RETURN,
  CODE_SPACE,
  CODE_TAB,
  TOKEN_EOF,
  TOKEN_EQUALITY,
  EofToken,
  TOKEN_TYPE_ID,
  TOKEN_TYPE_ILLEGAL,
  TOKEN_INEQUALITY,
  TOKEN_TYPE_INT,
  RESERVED_TOKEN,
  SINGLE_TOKEN,
  Token,
  decodeChunk,
  isLetter,
  isNumber,
  mergeChunks,
  Chunk,
  AnyIterable,
  SingleTokenType,
  ReservedTokenType,
} from '../common';
import { extractIterator } from '../utils';

export type ChunkLexer = AsyncGenerator<Token, EofToken, undefined>;

/**
 * Takes an async iterable stream of characters and spits out tokens
 * @arg source UTF-8/16 ONLY !!!
 * @returns 
 */
export async function* createChunkLexer(
  source: AnyIterable<Chunk>
): ChunkLexer {
  const iter = extractIterator(source);

  let result: IteratorResult<Chunk>;
  while (!(result = await iter.next()).done) {
    let chunk = result.value;
    for (let idx = 0; idx < chunk.length; idx++) {
      const char = chunk[idx];

      // As a bonus, this works with null terminator (a.k.a. "\0")
      if (!char) return TOKEN_EOF;

      if (
        char === CODE_SPACE ||
        char === CODE_TAB ||
        char === CODE_NEWLINE ||
        char === CODE_RETURN
      )
        continue;

      if (SINGLE_TOKEN.hasOwnProperty(char)) {
        yield SINGLE_TOKEN[char as SingleTokenType];
        continue;
      }

      if (char === CODE_EQUAL) {
        if (chunk[idx + 1] === CODE_EQUAL) {
          yield TOKEN_EQUALITY;
          idx += 1;
        } else {
          yield TOKEN_ASSIGNMENT;
        }
        continue;
      }

      if (char === CODE_BANG) {
        if (chunk[idx + 1] === CODE_EQUAL) {
          yield TOKEN_INEQUALITY;
          idx += 1;
        } else {
          yield TOKEN_BANG;
        }
        continue;
      }

      if (isNumber(char)) {
        const startIdx = idx;

        // Successively check if next chars are numbers
        // FIXME: improve chunk handling efficiency?...
        do {
          idx += 1; // Increment the index pointer
          // If the pointer is within bounds of the original chunk,
          // go onto the next char
          if (idx < chunk.length) continue;

          // If the pointer is out of bounds, request the next chunk
          const nextResult = await iter.next();

          // If there is a chunk, just add it to the original
          // and go onto the next char
          // (which is the first char of the next chunk, incidentally)
          if (!nextResult.done) {
            chunk = mergeChunks(chunk, nextResult.value);
            continue;
          }

          // If the iterator is done, yield the last token...
          yield {
            type: TOKEN_TYPE_INT,
            literal: decodeChunk(startIdx ? chunk.slice(startIdx) : chunk),
          };

          // ...and return the End of File token
          return TOKEN_EOF;
        } while (isNumber(chunk[idx]));

        yield {
          type: TOKEN_TYPE_INT,
          literal: decodeChunk(chunk.slice(startIdx, idx)),
        };

        continue;
      }

      if (isLetter(char)) {
        const startIdx = idx;

        // Successively check if next chars are letters
        // FIXME: See above similar code with `isNumber`
        do {
          idx += 1;
          if (idx < chunk.length) continue;

          const nextResult = await iter.next();

          if (!nextResult.done) {
            chunk = mergeChunks(chunk, nextResult.value);
            continue;
          }

          const literal = decodeChunk(startIdx ? chunk.slice(startIdx) : chunk);
          yield RESERVED_TOKEN.hasOwnProperty(literal)
            ? RESERVED_TOKEN[literal as ReservedTokenType]
            : { type: TOKEN_TYPE_ID, literal };

          return TOKEN_EOF;
        } while (isLetter(chunk[idx]));

        const literal = decodeChunk(chunk.slice(startIdx, idx));
        yield RESERVED_TOKEN.hasOwnProperty(literal)
          ? RESERVED_TOKEN[literal as ReservedTokenType]
          : { type: TOKEN_TYPE_ID, literal };

        continue;
      }

      yield { type: TOKEN_TYPE_ILLEGAL, literal: String.fromCharCode(char) };
    }
  }

  return TOKEN_EOF;
}

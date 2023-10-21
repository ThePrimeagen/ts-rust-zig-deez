import {
  CODE_A_LOWER,
  CODE_A_UPPER,
  CODE_NINE,
  CODE_UNDERSCORE,
  CODE_ZERO,
  CODE_Z_LOWER,
  CODE_Z_UPPER,
} from './codes';
import { Chunk } from './types';

export const isNumber = (char: number) => char >= CODE_ZERO && char <= CODE_NINE;

export const isLetter = (char: number) => (
  char === CODE_UNDERSCORE ||
  (char >= CODE_A_UPPER && char <= CODE_Z_UPPER) ||
  (char >= CODE_A_LOWER && char <= CODE_Z_LOWER)
);

export const mergeChunks = (lhChunk: Chunk, rhChunk: Chunk): Chunk => {
  const lhLength = lhChunk.length;
  if (!lhLength) return rhChunk;
  const rhLength = rhChunk.length;
  if (!rhLength) return lhChunk;

  const newChunk = new Uint8Array(lhLength + rhLength);
  newChunk.set(lhChunk);
  newChunk.set(rhChunk, lhLength);
  return newChunk;
};

const decoder = new TextDecoder();
export const decodeChunk = (chunk: Chunk) => decoder.decode(chunk);

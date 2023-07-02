import {
  AnyIterable,
  CODE_NEWLINE,
  CODE_RETURN,
  Chunk,
  mergeChunks,
} from '../common';

import { extractIterator } from './extractIterator';

export async function* chunkByNewline(
  source: AnyIterable<Chunk>
): AsyncGenerator<Chunk, void, undefined> {
  const iter = extractIterator(source);

  let result: IteratorResult<Chunk>;
  let chunk: Chunk = new Uint8Array();
  while (!(result = await iter.next()).done) {
    // If the previous chunk remains, merge it with the next one
    // The function doesn't copy/allocate if one of the chunks has zero length
    chunk = mergeChunks(chunk, result.value);
    for (let idx = 0; idx < chunk.length; idx++) {
      const char = chunk[idx];

      // If no char or the char is null terminator (a.k.a. "\0"),
      // release the remaining chunk and return
      if (!char) {
        if (chunk.length) yield chunk;
        return;
      }

      if (char !== CODE_RETURN && char !== CODE_NEWLINE) continue;

      // If the char is "\n" or "\r" followed by "\n",
      // release the current chunk without newline chars,
      // then shift the buffer view (to avoid copying)
      // and reset the index pointer
      yield chunk.slice(0, idx);

      if (char === CODE_NEWLINE) {
        chunk = chunk.subarray(idx);
        idx = 0;
        continue;
      }

      // At this point the char is definitely "\r"
      // and we need to check whether the next char is "\n",
      // but the next char may be in the next chunk,
      // so we need to handle that scenario
      if (idx + 1 !== chunk.length) {
        const offset = chunk[idx + 1] === CODE_NEWLINE ? 1 : 0;
        chunk = chunk.subarray(idx + offset);
        idx = 0;
        continue;
      }

      const nextResult = await iter.next();
      if (nextResult.done) return;

      const nextChunk = nextResult.value;
      chunk = nextChunk[0] === CODE_NEWLINE ? nextChunk.subarray(1) : nextChunk;
      idx = 0;
    }
  }

  // If the chunk remains, release it
  if (chunk.length) yield chunk;
  return;
}

import {
  AnyIterable,
  CODE_GARBO_BACKSPACE,
  Chunk,
} from '../common';

const isNotGarbo = (char: number) => char !== CODE_GARBO_BACKSPACE;

export async function* skipGarboChars(
  source: AnyIterable<Chunk>
): AsyncGenerator<Chunk, void, undefined> {
  for await (const chunk of source) yield chunk.filter(isNotGarbo);
}

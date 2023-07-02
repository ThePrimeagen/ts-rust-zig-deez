#!/usr/bin/env ts-node

import { Interface, createInterface } from 'readline';

import { createChunkLexer, pipe } from '.';

// For whatever reason,
// readline doesn't have an option to return lines as Buffers...
// And on top of that it's [Symbol.asyncIterator] doesn't work...
// Preposterous, I say!!!1!!!
async function* fixDumbReadlineBugs(readline: Interface) {
  let shouldContinue = true;
  readline.once('close', () => void (shouldContinue = false));
  while (shouldContinue)
    yield new Promise((res) =>
      readline.once('line', (line) => res(Buffer.from(line)))
    );
}

const main = async () => {
  console.clear();

  const readline = createInterface({
    input: process.stdin,
    output: process.stdout,
  }).once('close', () => console.log('Primus sucks!!1!!'));

  const lexer = pipe(readline, fixDumbReadlineBugs, createChunkLexer);

  for await (const token of lexer)
    readline.write(`{ type: "${token.type}", literal: "${token.literal}" }\n`);
};

main();

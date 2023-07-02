import { Readable } from 'stream';
import { expect, test } from '@jest/globals';

import { chunkByNewline } from '../chunkByNewline';
import { zipAsync } from '../zipAsync';

test('test chunks interlaced with newlines', async () => {
  const stream = Readable.from([
    Buffer.from('siodfjsoaidfdij\njfsdoijfoi'),
    Buffer.from('siuadfhuisdhifuhiusdhfiuahd'),
    Buffer.from('\nsojdifjaiof\rosjadiofjs\r'),
    Buffer.from('\nsijdfiojidosjafoidfjoisdj'),
  ]);

  const newlineStream = chunkByNewline(stream);

  const testLines = [
    Buffer.from('siodfjsoaidfdij'),
    Buffer.from('jfsdoijfoisiuadfhuisdhifuhiusdhfiuahd'),
    Buffer.from('sojdifjaiof'),
    Buffer.from('osjadiofjs'),
    Buffer.from('sijdfiojidosjafoidfjoisdj'),
  ];

  for await (const [lineResult, testLineResult] of zipAsync(
    newlineStream,
    testLines
  )) {
    expect(lineResult).toEqual(testLineResult);
  }
});

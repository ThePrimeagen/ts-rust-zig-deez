import { expect, test } from '@jest/globals';

import { zipAsync } from '../zipAsync';

test('test multiple iterables', async () => {
  const array = [...'abc'];
  const set = new Set(array);
  const asyncGenerator = (async function* () {
    for (const item of array) yield item;
  })();

  const testIter = array[Symbol.iterator]();

  for await (const results of zipAsync(array, set, asyncGenerator)) {
    const test = testIter.next().value as string;
    const values = results.map(({ value }) => value);
    expect(values).toEqual(new Array(3).fill(test));
  }
});

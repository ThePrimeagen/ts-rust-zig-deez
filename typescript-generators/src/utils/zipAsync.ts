import { AnyIterable } from '../common';
import { extractIterator } from './extractIterator';

type AnyIterableTuple = [AnyIterable, AnyIterable, ...AnyIterable[]];

type AnyIterableItem<TIterable extends AnyIterable> =
  TIterable extends AnyIterable<infer T> ? T : unknown;

type AnyIterableResults<TIterables extends AnyIterableTuple> = {
  [index in keyof TIterables]: IteratorResult<
    AnyIterableItem<TIterables[index]>,
    void
  >;
};

type ZipAsyncGenerator<TIterables extends AnyIterableTuple> = AsyncGenerator<
  AnyIterableResults<TIterables>,
  void,
  undefined
>;

export async function* zipAsync<TIterables extends AnyIterableTuple>(
  ...iterables: TIterables
): ZipAsyncGenerator<TIterables> {
  const length = iterables.length;
  const iters = iterables.map(extractIterator);

  const returns: IteratorReturnResult<void>[] = new Array(length).fill(null);
  while (returns.every(Boolean)) {
    const results = new Array(length) as AnyIterableResults<TIterables>;

    for (let idx = 0; idx < length; idx++) {
      if (returns[idx]) {
        results[idx] = returns[idx];
        continue;
      }

      const result = await iters[idx].next();
      if (result.done) returns[idx] = result;
      results[idx] = result;
    }

    yield results;
  }
}

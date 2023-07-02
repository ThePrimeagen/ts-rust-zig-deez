import { AnyIterable } from "../common";

export const extractIterator = <T>(
  iterable: AnyIterable<T>
): Iterator<T> | AsyncIterator<T> => {
  if ((iterable as Iterable<T>)[Symbol.iterator])
    return (iterable as Iterable<T>)[Symbol.iterator]();

  if ((iterable as AsyncIterable<T>)[Symbol.asyncIterator])
    return (iterable as AsyncIterable<T>)[Symbol.asyncIterator]();

  throw new TypeError('Arguments should implement (async) iterator protocol');
};

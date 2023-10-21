interface ReadableStream<R> {
  [Symbol.asyncIterator](
    this: ReadableStream,
    options?: { preventCancel?: boolean }
  ): AsyncGenerator<R, void, undefined>;
}

// FIXME: No idea whether this is actually conformant
if (!ReadableStream.prototype[Symbol.asyncIterator])
  ReadableStream.prototype[Symbol.asyncIterator] = async function* ({
    preventCancel = false,
  } = {}) {
    const reader = this.getReader();
    try {
      while (true) {
        const result = await reader.read();
        if (result.done) {
          reader.releaseLock();
          return result.value;
        }
        yield result.value;
      }

    } catch (error) {
      await reader.cancel(error);
      throw error;

    } finally {
      if (!preventCancel) await reader.cancel();
      return reader.releaseLock();
    }
  };

# MonkeyLang Interpreter in Haskell

## Quickstart

To test the application you can use the Makefile commands.

```console
make docker-ready
```

Small cli tool that allows to switch between the different lexer
implementations and tokenize from stdin. (basic, monad, state, parsec)

```console
cat program.monkey | cabal run haskell basic
```

## Lexer

### Basic

`Lexer.Basic` is an attempt to make a very simple implementation of a lexer
using only pattern matching and iterating over a string.

### Monad

`Lexer.Monad` is an example of how you would implement a Lexer using
Transformer Monads. `Lexer.State` is the actual implementation using a State
Monad. This one feels the most similar to the book implementation.

### Megaparsec

`Lexer.Parsec` is an implementation of a lexer using the megaparsec library.
For some reason it feels really slow.

Overall I think it is cool to see different implementations for how to
implement lexers.

## Some weird things

1. How to deal with `dist-newstyle` when building with docker? Since the
   Makefile mounts the `$pwd` to the container it generates garbage files in
   the host filesystem. My workaround was to run `cabal clean` at the end, but
   is there a better way?

## Contributions

Any changes in terms of performance, refactoring, adding other lexers are
welcome.

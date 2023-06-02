# MonkeyLang Interpreter in Haskell

## Quickstart

To test the application you can use the Makefile commands as described in the
repo readme.

```console
make docker-ready
```

Small cli tool that allows to switch between the different lexer
implementations and tokenize from stdin. (basic, monad, state, parsec)

```console
cat program.monkey | cabal run haskell basic
```

## Tools used

- [Fourmolu](https://hackage.haskell.org/package/fourmolu) to format code
- [HLint](https://hackage.haskell.org/package/hlint) to check code
- [Cabal](https://hackage.haskell.org/package/Cabal) the build tool

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

### Lens

`Lexer.Lens` is an implementation of a lexer using State and Lens.

## Why use certain packages?

### [Control.Lens](https://hackage.haskell.org/package/lens-5.2.2/docs/Control-Lens.html)

Record syntax in Haskell is not very user friendly. Reading and updating fields
is verbose and using lenses makes it more like to to other languages where
`recored^.field` is equivalent to `record.field` and `record & field .~ value`
is equivalent to `record.field = value`

For example given a record

```haskell
  Lexer
    { input        :: String
    , position     :: Int
    , readPosition :: Int
    , ch           :: Char
    }
```

if we have a lexer record

```haskell
lexer = Lexer { input = "input", position = 0, readPosition = 0, ch = 'a' }
```

and we wanted to increment the `position` field we would have to do something like

```haskell
lexer' = lexer { position = position lexer + 1 }
```

and we wanted to increment the `readPosition`  field we would have to do

```haskell
lexer' = lexer { readPosition = readPosition lexer + 1 }
```

and if we wanted to update the `ch` field to a 'b' we would have to do something like

```haskell
lexer' = lexer { ch = 'b' }
```

and if we wanted to replace the `input` field with only its first two characters (unsafe) we would have to do something like

```haskell
lexer' = lexer { input = take 2 (input lexer) }
```

so this entire update was

```haskell
lexer' = lexer { input = take 2 (input lexer), position = position lexer + 1, readPosition = readPosition lexer + 1, ch = 'b' }
```

with lenses we can do this

```haskell
lexer' = lexer & input %~(take 2) & position +~1 & readPosition +~1 & ch .~ 'b'
```

This seems trivial here but when if we have a record with many fields it
becomes very verbose and hard to read particularly with nested records.

### [Data.ByteString.Char8](https://hackage.haskell.org/package/bytestring-0.11.4.0/docs/Data-ByteString-Char8.html)

Haskell `String` type is actually a linked list of `char`. This is not very
performant, e.g., `length` and indexing `(!!)` operations are O(N). Also,
indexing is unsafe (can fail) with crafting or importing safe operations. With
`ByteString` we have O(1) operations and built in safe indexing that return
`Maybe` types (Option in Rust).

## [Control.Monad.Trans.State](https://hackage.haskell.org/package/transformers-0.6.1.0/docs/Control-Monad-Trans-State.html)

since we are working with immutable variables, we have to keep passing the
state around. You can see this in the `OCaml` version where `lexer` is being
parsed around recursively and returned in a tuple?.

The `state` monad allows us to obfuscate the passing around of the state and
perform updates on the state within a function without the need for new
variable names.

```haskell
getToken :: (Lexer, Token) -> (Lexer, Token)
getToken (lexer, token) = (lexer'', token')
  where
    lexer'            = skipWhitespace lexer
    (lexer'', token') =    -- tracking the state of the lexer is a pain
      case lexer'^.ch of   -- imagine if we were doing multiple updates
        '+' -> (readChar lexer', Token PLUS)
        _   -> (readChar lexer', Token EOF)
```

becomes

```haskell
getToken :: State Lexer Token
getToken = do              -- no need to manaully track the state & value of the lexer
  modify skipWhitespace    -- we can modify state with get, put and modify
  lexer <- get
  case lexer^.ch of
    '+' -> modify readchar >> pure (Token PLUS)
    _   -> modify readcahr >> pure (Token EOF)
```

## Some weird things

1. How to deal with `dist-newstyle` when building with docker? Since the
   Makefile mounts the `$pwd` to the container it generates garbage files in
   the host filesystem. My workaround was to run `cabal clean` at the end, but
   is there a better way?

## Contributions

Any changes in terms of performance, refactoring, adding other lexers are welcome.

# Lexer

- complete noob attempt at lexer in Haskell
- changes and rewrites will be very much appreciated

- find bellow a list of imports used and why.

## [Control.Lens](https://hackage.haskell.org/package/lens-5.2.2/docs/Control-Lens.html)

- record syntax in Haskell  is not very good
- reading and updating fields is verbose and using lenses makes it more like to to other languages where `recored^.field` is equivalent to `record.field` and `record & field .~ value` is equivalent to `record.field = value`

for example given a record

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

This seems trivial here but when if we have a record with many fields it becomes very verbose and hard to read particularly with nested records.

## [Data.ByteString.Char8](https://hackage.haskell.org/package/bytestring-0.11.4.0/docs/Data-ByteString-Char8.html)

Haskell `String` type is actually a list of `char`.
This is not very performant, e.g., `length` and indexing `(!!)` operations are O(N). Also, indexing is unsafe (can fail) with crafting or importing safe operations. With `ByteString` we have O(1) operations and built in safe indexing that return `Maybe` types (Option in Rust).

## [Data.Maybe](https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-Maybe.html)

Used to unwrap `Maybe` type (with default in our case).

## [Data.Char](https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-Char.html)

Used  for built in `isSpace` , `isDigit`, `isLetter`

## [Control.Monad.Trans.State](https://hackage.haskell.org/package/transformers-0.6.1.0/docs/Control-Monad-Trans-State.html)

since we are working with immutable variables, we have to keep passing the state around. You can see this in the `OCaml` version where `lexer` is being parsed around recursively and returned in a tuple?.

The `state` monad allows us to obfuscate the passing around of the state and perform updates on the state within a function without the need for new variable names.

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

## External - Make.cmd

- used [HIndent](https://hackage.haskell.org/package/hindent) to format code - don't like it
- used [HLint](https://hackage.haskell.org/package/hlint) to check code - don't like it

module Test.Main where

import Prelude

import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Lexer (parser)
import Parsing (runParser)
import Parsing.Combinators.Array (many1)
import Parsing.String.Basic (whiteSpace)
import Test.Assert (assertEqual')
import Token (Token(..))

main :: Effect Unit
main = do
  let input = """
    let five = 5;
    let ten = 10;
    let add = fn(x, y) {
        x + y;
    };
    let result = add(five, ten);
  """
  
  let expected = NonEmptyArray [ 
      Let, (Ident "five"), Equal, (Int 5.0), Semicolon, 
      Let, (Ident "ten"), Equal, (Int 10.0), Semicolon, 
      Let, (Ident "add"), Equal, (Ident "fn"), LParen, (Ident "x"), Comma, (Ident "y"), RParen, LSquirly, 
        (Ident "x"), Plus, (Ident "y"), Semicolon, 
      RSquirly, Semicolon, 
      Let, (Ident "result"), Equal, (Ident "add"), LParen, (Ident "five"), Comma, (Ident "ten"), RParen, Semicolon 
  ]
  
  let
    output = runParser input $ many1 (whiteSpace *> parser)

  case output of
    Right actual -> do
      assertEqual' "Parsed tokens did not match exptected output" {actual, expected }

    Left error -> do
      log "Parsing Error"
      throw $ show error

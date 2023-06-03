module Data.LexerSpec
  ( spec
  ) where

import Lexer      (Token(..), TokenType(..), tokenize)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Test.Hspec.Spec
spec = do
  Test.Hspec.describe "getTokens" $ do
    Test.Hspec.it "handles operators" $ do
      let testInput = "=+(){},;"
          tokens = tokenize testInput
          expectedTokens =
            [ Token EQUAL
            , Token PLUS
            , Token LPAREN
            , Token RPAREN
            , Token LBRACE
            , Token RBRACE
            , Token COMMA
            , Token SEMICOLON
            , Token EOF
            ]
      tokens `shouldBe` expectedTokens
  Test.Hspec.describe "getTokens" $ do
    Test.Hspec.it "handles simple program" $ do
      let testInput =                                -- is there a better way to handle multiline strings?
            "let five = 5;\n\
            \let ten = 10;\n\
            \let add = fn(x, y) {\n\
            \  x + y;\n\
            \};\n\
            \let result = add(five, ten);"
          tokens = tokenize testInput
          expectedTokens =
            [ Token LET
            , Token (IDENT "five")
            , Token EQUAL
            , Token (INT "5")
            , Token SEMICOLON
            , Token LET
            , Token (IDENT "ten")
            , Token EQUAL
            , Token (INT "10")
            , Token SEMICOLON
            , Token LET
            , Token (IDENT "add")
            , Token EQUAL
            , Token FUNCTION
            , Token LPAREN
            , Token (IDENT "x")
            , Token COMMA
            , Token (IDENT "y")
            , Token RPAREN
            , Token LBRACE
            , Token (IDENT "x")
            , Token PLUS
            , Token (IDENT "y")
            , Token SEMICOLON
            , Token RBRACE
            , Token SEMICOLON
            , Token LET
            , Token (IDENT "result")
            , Token EQUAL
            , Token (IDENT "add")
            , Token LPAREN
            , Token (IDENT "five")
            , Token COMMA
            , Token (IDENT "ten")
            , Token RPAREN
            , Token SEMICOLON
            , Token EOF
            ]
      tokens `shouldBe` expectedTokens

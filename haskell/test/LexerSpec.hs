module LexerSpec (spec) where

import Data.List qualified
import Lexer (Token (..), parseLine, token)
import Parser (ParserState (..), many1, runParser)
import Test.Hspec (SpecWith, describe, it, shouldBe)

spec :: SpecWith ()
spec = do
  describe "lexerSpec" $ do
    let symbolsInput = "=+(){},;"

    let codeSnippetInput =
          Data.List.unlines
            [ "let five = 5;",
              "let ten = 10;",
              "let add = fn(x, y) {",
              "x + y;",
              "};",
              "let result = add(five, ten);"
            ]

    let gibberishCodeSnippet =
          codeSnippetInput
            <> Data.List.unlines ["!-/*5;", "5 < 10 > 5;"]

    let elseIfBoolCodeSnippet =
          gibberishCodeSnippet
            <> Data.List.unlines
              [ "if (5 < 10) {",
                "  return true;",
                "} else {",
                "  return false;",
                "}"
              ]

    let twoCharacterSymbolCodeSnippet =
          elseIfBoolCodeSnippet
            <> Data.List.unlines
              [ "10 == 10;",
                "10 != 9;"
              ]

    it "will lex symbols" $ do
      shouldBe
        (runParser (many1 token) symbolsInput)
        ( Right
            ( [Assign, Plus, LParen, RParen, LBrace, RBrace, Comma, Semicolon],
              ParserState
                { input = "",
                  position = 8,
                  ch = Just ';'
                }
            )
        )

    it "will lex a line" $ do
      shouldBe
        (runParser parseLine "let five=5;\nlet six=6;\n")
        ( Right
            ( [Let, Ident "five", Assign, IntLiteral 5, Semicolon],
              ParserState
                { input = "let six=6;\n",
                  position = 11,
                  ch = Just '\n'
                }
            )
        )

    it "will lex the code snippet" $
      shouldBe
        (runParser (many1 parseLine) codeSnippetInput)
        ( Right
            ( [ [Let, Ident "five", Assign, IntLiteral 5, Semicolon],
                [Let, Ident "ten", Assign, IntLiteral 10, Semicolon],
                [Let, Ident "add", Assign, Function, LParen, Ident "x", Comma, Ident "y", RParen, LBrace],
                [Ident "x", Plus, Ident "y", Semicolon],
                [RBrace, Semicolon],
                [Let, Ident "result", Assign, Ident "add", LParen, Ident "five", Comma, Ident "ten", RParen, Semicolon]
              ],
              ParserState {input = "", position = 71, ch = Just '\n'}
            )
        )

    it "will lex the gibberish code snippet" $
      shouldBe
        (runParser (many1 parseLine) gibberishCodeSnippet)
        ( Right
            ( [ [Let, Ident "five", Assign, IntLiteral 5, Semicolon],
                [Let, Ident "ten", Assign, IntLiteral 10, Semicolon],
                [Let, Ident "add", Assign, Function, LParen, Ident "x", Comma, Ident "y", RParen, LBrace],
                [Ident "x", Plus, Ident "y", Semicolon],
                [RBrace, Semicolon],
                [Let, Ident "result", Assign, Ident "add", LParen, Ident "five", Comma, Ident "ten", RParen, Semicolon],
                [Bang, Minus, Slash, Asterisk, IntLiteral 5, Semicolon],
                [IntLiteral 5, LessThan, IntLiteral 10, GreaterThan, IntLiteral 5, Semicolon]
              ],
              ParserState {input = "", position = 86, ch = Just '\n'}
            )
        )

    it "will lex if,else,boolean code snippet" $
      shouldBe
        (runParser (many1 parseLine) elseIfBoolCodeSnippet)
        ( Right
            ( [ [Let, Ident "five", Assign, IntLiteral 5, Semicolon],
                [Let, Ident "ten", Assign, IntLiteral 10, Semicolon],
                [Let, Ident "add", Assign, Function, LParen, Ident "x", Comma, Ident "y", RParen, LBrace],
                [Ident "x", Plus, Ident "y", Semicolon],
                [RBrace, Semicolon],
                [Let, Ident "result", Assign, Ident "add", LParen, Ident "five", Comma, Ident "ten", RParen, Semicolon],
                [Bang, Minus, Slash, Asterisk, IntLiteral 5, Semicolon],
                [IntLiteral 5, LessThan, IntLiteral 10, GreaterThan, IntLiteral 5, Semicolon],
                [If, LParen, IntLiteral 5, LessThan, IntLiteral 10, RParen, LBrace],
                [Return, BoolLiteral True, Semicolon],
                [RBrace, Else, LBrace],
                [Return, BoolLiteral False, Semicolon],
                [RBrace]
              ],
              ParserState {input = "", position = 130, ch = Just '\n'}
            )
        )

    it "will lex two charater symbol code snippet" $
      shouldBe
        (runParser (many1 parseLine) twoCharacterSymbolCodeSnippet)
        ( Right
            ( [ [Let, Ident "five", Assign, IntLiteral 5, Semicolon],
                [Let, Ident "ten", Assign, IntLiteral 10, Semicolon],
                [Let, Ident "add", Assign, Function, LParen, Ident "x", Comma, Ident "y", RParen, LBrace],
                [Ident "x", Plus, Ident "y", Semicolon],
                [RBrace, Semicolon],
                [Let, Ident "result", Assign, Ident "add", LParen, Ident "five", Comma, Ident "ten", RParen, Semicolon],
                [Bang, Minus, Slash, Asterisk, IntLiteral 5, Semicolon],
                [IntLiteral 5, LessThan, IntLiteral 10, GreaterThan, IntLiteral 5, Semicolon],
                [If, LParen, IntLiteral 5, LessThan, IntLiteral 10, RParen, LBrace],
                [Return, BoolLiteral True, Semicolon],
                [RBrace, Else, LBrace],
                [Return, BoolLiteral False, Semicolon],
                [RBrace],
                [IntLiteral 10, EqSymbol, IntLiteral 10, Semicolon],
                [IntLiteral 10, NotEqSymbol, IntLiteral 9, Semicolon]
              ],
              ParserState {input = "", position = 143, ch = Just '\n'}
            )
        )

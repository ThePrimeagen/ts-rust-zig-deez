module LexerTest where

import Lexer (Token (..), mkLexer, nextToken)
import Test.HUnit

tokenize :: String -> [Token]
tokenize = go . mkLexer
  where
    go lexer = case nextToken lexer of
        (_, Eof) -> [Eof]
        (lexer', token) -> token : go lexer'

testToknizeInitial :: Test
testToknizeInitial = TestCase $ do
    let input = "=+(){},;"
    let expected = [Assign, Plus, LParen, RParen, LSquirly, RSquirly, Comma, Semicolon, Eof]
    let tokens = tokenize input
    assertEqual "testToknizeInitial" expected tokens

testTokenizeBasic :: Test
testTokenizeBasic = TestCase $ do
    let input = unlines ["let five = 5;", "let ten = 10;", "let add = fn(x, y) {", "x + y;", "};", "let result = add(five, ten);"]
    let expected = [Let, Ident "five", Assign, Int "5", Semicolon, Let, Ident "ten", Assign, Int "10", Semicolon, Let, Ident "add", Assign, Function, LParen, Ident "x", Comma, Ident "y", RParen, LSquirly, Ident "x", Plus, Ident "y", Semicolon, RSquirly, Semicolon, Let, Ident "result", Assign, Ident "add", LParen, Ident "five", Comma, Ident "ten", RParen, Semicolon, Eof]
    let tokens = tokenize input
    assertEqual "testTokenizeBasic" expected tokens

testTokenizeExtended :: Test
testTokenizeExtended = TestCase $ do
    let input = unlines ["let five = 5;", "", "let ten = 10;", "", "let add = fn(x, y) {", "    x + y;", "};", "", "let result = add(five, ten);", "!-/*5;", "5 < 10 > 5;", "", "if (5 < 10) {", "    return true;", "} else {", "    return false;", "}"]
    let expected = [Let, Ident "five", Assign, Int "5", Semicolon, Let, Ident "ten", Assign, Int "10", Semicolon, Let, Ident "add", Assign, Function, LParen, Ident "x", Comma, Ident "y", RParen, LSquirly, Ident "x", Plus, Ident "y", Semicolon, RSquirly, Semicolon, Let, Ident "result", Assign, Ident "add", LParen, Ident "five", Comma, Ident "ten", RParen, Semicolon, Bang, Minus, Slash, Asterisk, Int "5", Semicolon, Int "5", LessThan, Int "10", GreaterThan, Int "5", Semicolon, If, LParen, Int "5", LessThan, Int "10", RParen, LSquirly, Return, TrueTok, Semicolon, RSquirly, Else, LSquirly, Return, FalseTok, Semicolon, RSquirly, Eof]
    let tokens = tokenize input
    assertEqual "testTokenizeExtended" expected tokens

testTokenizeEqual :: Test
testTokenizeEqual = TestCase $ do
    let input = "a == b != c"
    let expected = [Ident "a", Equal, Ident "b", NotEqual, Ident "c", Eof]
    let tokens = tokenize input
    assertEqual "testTokenizeEqual" expected tokens

lexerTests :: Test
lexerTests =
    TestList
        [ TestLabel "testToknizeInitial" testToknizeInitial
        , TestLabel "testTokenizeBasic" testTokenizeBasic
        , TestLabel "testTokenizeExtended" testTokenizeExtended
        , TestLabel "testTokenizeEqual" testTokenizeEqual
        ]

module LexerTest where

import Lexer (Token (..), tokenize)
import Test.HUnit

testGetNextToken :: Test
testGetNextToken = TestCase $ do
    let input = "=+(){},;"
    let expected = [Equal, Plus, LParen, RParen, LSquirly, RSquirly, Comma, Semicolon]
    let tokens = take (length expected) $ tokenize input
    assertEqual "testGetNextToken" expected tokens

testGetNextComplete :: Test
testGetNextComplete = TestCase $ do
    let input = unlines ["let five = 5;", "let ten = 10;", "let add = fn(x, y) {", "x + y;", "};", "let result = add(five, ten);"]
    let expected = [Let, Ident "five", Equal, Int "5", Semicolon, Let, Ident "ten", Equal, Int "10", Semicolon, Let, Ident "add", Equal, Function, LParen, Ident "x", Comma, Ident "y", RParen, LSquirly, Ident "x", Plus, Ident "y", Semicolon, RSquirly, Semicolon, Let, Ident "result", Equal, Ident "add", LParen, Ident "five", Comma, Ident "ten", RParen, Semicolon]
    let tokens = take (length expected) $ tokenize input
    assertEqual "testGetNextComplete" expected tokens

lexerTests :: Test
lexerTests = TestList [TestLabel "testGetNextToken" testGetNextToken, TestLabel "testGetNextComplete" testGetNextComplete]

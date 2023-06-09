module LexerTest where

import Test.HUnit
import Token (Token (..), Tokenizer)

testToknizeInitial :: Tokenizer -> Test
testToknizeInitial tokenize = TestCase $ do
    let input = "=+(){},;"
    let expected = [Assign, Plus, LParen, RParen, LSquirly, RSquirly, Comma, Semicolon, Eof]
    let tokens = tokenize input
    assertEqual "testToknizeInitial" expected tokens

testTokenizeBasic :: Tokenizer -> Test
testTokenizeBasic tokenize = TestCase $ do
    let input = unlines ["let five = 5;", "let ten = 10;", "let add = fn(x, y) {", "x + y;", "};", "let result = add(five, ten);"]
    let expected = [Let, Ident "five", Assign, Int "5", Semicolon, Let, Ident "ten", Assign, Int "10", Semicolon, Let, Ident "add", Assign, Function, LParen, Ident "x", Comma, Ident "y", RParen, LSquirly, Ident "x", Plus, Ident "y", Semicolon, RSquirly, Semicolon, Let, Ident "result", Assign, Ident "add", LParen, Ident "five", Comma, Ident "ten", RParen, Semicolon, Eof]
    let tokens = tokenize input
    assertEqual "testTokenizeBasic" expected tokens

testTokenizeExtended :: Tokenizer -> Test
testTokenizeExtended tokenize = TestCase $ do
    let input = unlines ["let five = 5;", "", "let ten = 10;", "", "let add = fn(x, y) {", "    x + y;", "};", "", "let result = add(five, ten);", "!-/*5;", "5 < 10 > 5;", "", "if (5 < 10) {", "    return true;", "} else {", "    return false;", "}"]
    let expected = [Let, Ident "five", Assign, Int "5", Semicolon, Let, Ident "ten", Assign, Int "10", Semicolon, Let, Ident "add", Assign, Function, LParen, Ident "x", Comma, Ident "y", RParen, LSquirly, Ident "x", Plus, Ident "y", Semicolon, RSquirly, Semicolon, Let, Ident "result", Assign, Ident "add", LParen, Ident "five", Comma, Ident "ten", RParen, Semicolon, Bang, Minus, Slash, Asterisk, Int "5", Semicolon, Int "5", LessThan, Int "10", GreaterThan, Int "5", Semicolon, If, LParen, Int "5", LessThan, Int "10", RParen, LSquirly, Return, TrueTok, Semicolon, RSquirly, Else, LSquirly, Return, FalseTok, Semicolon, RSquirly, Eof]
    let tokens = tokenize input
    assertEqual "testTokenizeExtended" expected tokens

testTokenizeEqual :: Tokenizer -> Test
testTokenizeEqual tokenize = TestCase $ do
    let input = "a == b ? != c"
    let expected = [Ident "a", Equal, Ident "b", Illegal, NotEqual, Ident "c", Eof]
    let tokens = tokenize input
    assertEqual "testTokenizeEqual" expected tokens

testTokenizeBig :: Tokenizer -> Test
testTokenizeBig tokenize = TestCase $ do
    let input = unlines ["let five = 5;", "", "let ten = 10;", "", "let add = fn(x, y) {", "    x + y;", "};", "", "let result = add(five, ten);", "1 + 2 * 3 / 5 - -4 == 7;", "5 < 10 == !false;", "", "if (5 < 10) {", "    return true;", "} else {", "    return false;", "};", "", "fn(x, y) { if (true) { x } else { x } } (1, 2);"]
    let expected = [Let, Ident "five", Assign, Int "5", Semicolon, Let, Ident "ten", Assign, Int "10", Semicolon, Let, Ident "add", Assign, Function, LParen, Ident "x", Comma, Ident "y", RParen, LSquirly, Ident "x", Plus, Ident "y", Semicolon, RSquirly, Semicolon, Let, Ident "result", Assign, Ident "add", LParen, Ident "five", Comma, Ident "ten", RParen, Semicolon, Int "1", Plus, Int "2", Asterisk, Int "3", Slash, Int "5", Minus, Minus, Int "4", Equal, Int "7", Semicolon, Int "5", LessThan, Int "10", Equal, Bang, FalseTok, Semicolon, If, LParen, Int "5", LessThan, Int "10", RParen, LSquirly, Return, TrueTok, Semicolon, RSquirly, Else, LSquirly, Return, FalseTok, Semicolon, RSquirly, Semicolon, Function, LParen, Ident "x", Comma, Ident "y", RParen, LSquirly, If, LParen, TrueTok, RParen, LSquirly, Ident "x", RSquirly, Else, LSquirly, Ident "x", RSquirly, RSquirly, LParen, Int "1", Comma, Int "2", RParen, Semicolon, Eof]
    let tokens = tokenize input
    assertEqual "testTokenizeBig" expected tokens

lexerTests :: Tokenizer -> Test
lexerTests tokenize =
    TestList
        [ TestLabel "testToknizeInitial" (testToknizeInitial tokenize)
        , TestLabel "testTokenizeBasic" (testTokenizeBasic tokenize)
        , TestLabel "testTokenizeExtended" (testTokenizeExtended tokenize)
        , TestLabel "testTokenizeEqual" (testTokenizeEqual tokenize)
        , TestLabel "testTokenizeBig" (testTokenizeBig tokenize)
        ]

module ParserTest where

import Test.HUnit
import Token (Token(..))
import AST (Ast, Program(..), Statement(..), Expression(..), Identifier(..))

testAstInitial :: Ast -> Test
testAstInitial ast = TestCase $ do
    let tokens = [Let, Ident "a", Assign, Ident "b"]
    let expected = Program [LetStatement (Identifier "a") (IdentExpr (Identifier "b"))]
    let tree = ast tokens
    assertEqual "testAstInitial" tree (Right expected)

parserTests :: Ast -> Test
parserTests ast =
    TestList
        [ TestLabel "testAstInitial" (testAstInitial ast)
        ]


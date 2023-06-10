module ParserTest where

import AST (Ast, Block (..), Expression (..), Identifier (..), Program (..), Statement (..))
import Test.HUnit
import Token (Token (..))

testAstLet :: Ast -> Test
testAstLet ast = TestCase $ do
    let tokens = [Let, Ident "five", Assign, Int "5", Semicolon, Eof]
    let expected = Program [LetStatement (Identifier "five") (IntLiteral 5)]
    let tree = ast tokens
    assertEqual "testAstLet" expected tree

testAstFn :: Ast -> Test
testAstFn ast = TestCase $ do
    let tokens = [Let, Ident "add", Assign, Function, LParen, Ident "x", Comma, Ident "y", RParen, LSquirly, Ident "x", Plus, Ident "y", Semicolon, RSquirly, Semicolon, Eof]
    let expected = Program [LetStatement (Identifier "add") (FunctionExpr [Identifier "x", Identifier "y"] (Block [ExpressionStatement (AddExpr (IdentExpr (Identifier "x")) (IdentExpr (Identifier "y")))]))]
    let tree = ast tokens
    assertEqual "testAstFn" expected tree

tokenAstCall :: Ast -> Test
tokenAstCall ast = TestCase $ do
    let tokens = [Let, Ident "result", Assign, Ident "add", LParen, Ident "five", Comma, Ident "ten", RParen, Semicolon, Eof]
    let expect = Program [LetStatement (Identifier "result") (CallExpr (IdentExpr (Identifier "add")) [IdentExpr (Identifier "five"), IdentExpr (Identifier "ten")])]
    let tree = ast tokens
    assertEqual "tokenAstCall" expect tree

tokenAstArith :: Ast -> Test
tokenAstArith ast = TestCase $ do
    let tokens = [Int "1", Plus, Int "2", Asterisk, Int "3", Slash, Int "5", Minus, Minus, Int "4", Equal, Int "7", Semicolon, Eof]
    let expect = Program [ExpressionStatement (EqExpr (SubExpr (AddExpr (IntLiteral 1) (DivExpr (MulExpr (IntLiteral 2) (IntLiteral 3)) (IntLiteral 5))) (NegExpr (IntLiteral 4))) (IntLiteral 7))]
    let tree = ast tokens
    assertEqual "tokenAstArith" expect tree

tokenAstArith2 :: Ast -> Test
tokenAstArith2 ast = TestCase $ do
    let tokens = [Int "5", LessThan, Int "10", Equal, Bang, FalseTok, Semicolon, Eof]
    let expect = Program [ExpressionStatement (EqExpr (LtExpr (IntLiteral 5) (IntLiteral 10)) (NotExpr (BoolLiteral False)))]
    let tree = ast tokens
    assertEqual "tokenAstArith2" expect tree

tokenAstIf :: Ast -> Test
tokenAstIf ast = TestCase $ do
    let tokens = [If, LParen, Int "5", LessThan, Int "10", RParen, LSquirly, Return, TrueTok, Semicolon, RSquirly, Else, LSquirly, Return, FalseTok, Semicolon, RSquirly, Semicolon, Eof]
    let expect = Program [ExpressionStatement (IfExpr (LtExpr (IntLiteral 5) (IntLiteral 10)) (Block [ReturnStatement (BoolLiteral True)]) (Just (Block [ReturnStatement (BoolLiteral False)])))]
    let tree = ast tokens
    assertEqual "tokenAstIf" expect tree

tokenAstFnCallIf :: Ast -> Test
tokenAstFnCallIf ast = TestCase $ do
    let tokens = [Function, LParen, Ident "x", Comma, Ident "y", RParen, LSquirly, If, LParen, TrueTok, RParen, LSquirly, Ident "x", RSquirly, Else, LSquirly, Ident "x", RSquirly, RSquirly, LParen, Int "1", Comma, Int "2", RParen, Semicolon, Eof]
    let expect = Program [ExpressionStatement (CallExpr (FunctionExpr [Identifier "x", Identifier "y"] (Block [ExpressionStatement (IfExpr (BoolLiteral True) (Block [ExpressionStatement (IdentExpr (Identifier "x"))]) (Just (Block [ExpressionStatement (IdentExpr (Identifier "x"))])))])) [IntLiteral 1, IntLiteral 2])]
    let tree = ast tokens
    assertEqual "tokenAstFnCallIf" expect tree

parserTests :: Ast -> Test
parserTests ast =
    TestList
        [ TestLabel "testAstLet" (testAstLet ast)
        , TestLabel "testAstFn" (testAstFn ast)
        , TestLabel "tokenAstCall" (tokenAstCall ast)
        , TestLabel "tokenAstArith" (tokenAstArith ast)
        , TestLabel "tokenAstArith2" (tokenAstArith2 ast)
        , TestLabel "tokenAstIf" (tokenAstIf ast)
        , TestLabel "tokenAstFnCallIf" (tokenAstFnCallIf ast)
        ]

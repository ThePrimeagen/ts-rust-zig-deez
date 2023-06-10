import XCTest
@testable import Lexer

final class LexerTests: XCTestCase {

    func testNextToken() {
        var lexer = Lexer(input: """
        let five = 5;
        let ten = 10;

        let add = fn(x, y) {
            x + y;
        };

        let result = add(five, ten);
        """)

        let expectedTokens: [Token] = [
            .let,
            .ident("five"),
            .assign,
            .int(5),
            .semi,
            .let,
            .ident("ten"),
            .assign,
            .int(10),
            .semi,
            .let,
            .ident("add"),
            .assign,
            .function,
            .lParen,
            .ident("x"),
            .comma,
            .ident("y"),
            .rParen,
            .lSquirly,
            .ident("x"),
            .plus,
            .ident("y"),
            .semi,
            .rSquirly,
            .semi,
            .let,
            .ident("result"),
            .assign,
            .ident("add"),
            .lParen,
            .ident("five"),
            .comma,
            .ident("ten"),
            .rParen,
            .semi,
            .eof
        ]

        for expectedToken in expectedTokens {
            let nextToken = lexer.nextToken()
            XCTAssertEqual(nextToken, expectedToken)
        }
    }
    
    func testIdentifier() {
        var lexer = Lexer(input: "variable")
        let expectedToken = Token.ident("variable")
        let nextToken = lexer.nextToken()
        
        XCTAssertEqual(nextToken, expectedToken)
    }

    func testNumber() {
        var lexer = Lexer(input: "123")
        let expectedToken = Token.int(123)
        let nextToken = lexer.nextToken()
        
        XCTAssertEqual(nextToken, expectedToken)
    }
}

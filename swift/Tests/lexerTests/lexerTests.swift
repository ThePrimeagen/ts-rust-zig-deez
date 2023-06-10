import XCTest
@testable import Lexer

final class LexerTests: XCTestCase {

    func testNextToken() {
        var lexer = Lexer(input: "fn let = + ; ( ) { } ,")

        let expectedTokens: [Token] = [
            .function,
            .let,
            .assign,
            .plus,
            .semi,
            .lParen,
            .rParen,
            .lSquirly,
            .rSquirly,
            .comma,
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

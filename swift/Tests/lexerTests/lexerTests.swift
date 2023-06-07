import XCTest
@testable import Lexer

final class LexerTests: XCTestCase {

    func testNextToken() {
        let lexer = Lexer("fn let = + ; ( ) { } ,")

        let expectedTokens: [Token] = [
            Token(type: .function, literal: "fn"),
            Token(type: .let, literal: "let"),
            Token(type: .equal, literal: "="),
            Token(type: .plus, literal: "+"),
            Token(type: .semi, literal: ";"),
            Token(type: .lParen, literal: "("),
            Token(type: .rParen, literal: ")"),
            Token(type: .lSqirly, literal: "{"),
            Token(type: .rSqirly, literal: "}"),
            Token(type: .comma, literal: ","),
            Token(type: .eof, literal: "\0")
        ]

        for expectedToken in expectedTokens {
            let nextToken = lexer.nextToken()
            XCTAssertEqual(nextToken.type, expectedToken.type)
            XCTAssertEqual(nextToken.literal, expectedToken.literal)
        }
    }
    
    func testIdentifier() {
        let lexer = Lexer("variable")
        let expectedToken = Token(type: .ident, literal: "variable")
        let nextToken = lexer.nextToken()
        
        XCTAssertEqual(nextToken.type, expectedToken.type)
        XCTAssertEqual(nextToken.literal, expectedToken.literal)
    }

    func testNumber() {
        let lexer = Lexer("123")
        let expectedToken = Token(type: .int, literal: "123")
        let nextToken = lexer.nextToken()
        
        XCTAssertEqual(nextToken.type, expectedToken.type)
        XCTAssertEqual(nextToken.literal, expectedToken.literal)
    }
}

import Foundation

enum TokenType {
    case illegal, eof, ident, int, comma, semi, plus, equal, lParen, rParen, lSqirly, rSqirly, function, `let`
}

struct Token {
    let type: TokenType
    let literal: String
}

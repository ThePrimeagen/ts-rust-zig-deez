import Foundation

final class Lexer {

    var input: String
    var pos: Int
    var keywordMap: [String: Token]
    
    init(_ input: String) {
        self.input = input
        self.pos = 0
        self.keywordMap = [
            "fn": Token(type: .function, literal: "fn"),
            "let": Token(type: .let, literal: "let"),
        ]
    }
    
    func nextToken() -> Token {
        self.skipWhitespace()

        var token: Token
        var tokenType: TokenType = .illegal
        switch currentChar {
            case "{": tokenType = .lSqirly
            case "}": tokenType = .rSqirly
            case "(": tokenType = .lParen
            case ")": tokenType = .rParen
            case ",": tokenType = .comma
            case ";": tokenType = .semi
            case "+": tokenType = .plus
            case "=": tokenType = .equal
            case "\0": tokenType = .eof
            default: break
        }

        if tokenType == .eof {
            token = createToken(type: tokenType, literal: "eof")
        } else if isLetter(currentChar) {
            let ident = readIdentifier()
            if let keyword = keywordMap[ident] {
                return keyword
            } else {
                return createToken(type: .ident, literal: ident)
            }
        } else if isNumber(currentChar) {
            return createToken(type: .int, literal: self.readNumber())
        } else {
            token = createToken(type: tokenType, literal: String(currentChar))
        }
        
        advance()
        return token
    }

    private func advance() {
        if pos + 1 >= input.count {
            pos = -1
        } else {
            pos += 1
        }
    }
    
    private var currentChar: Character {
        if pos >= input.count || pos < 0 {
            return "\0"
        }
        let index = input.index(input.startIndex, offsetBy: self.pos)
        return input[index]
    }

    private func createToken(type: TokenType, literal: String) -> Token {
        return Token(type: type, literal: literal)
    }

    private func isLetter(_ c: Character) -> Bool {
        return c.isLetter || c == "_"
    }

    private func isNumber(_ c: Character) -> Bool {
        return c.isNumber
    }

    private func readNumber() -> String {
        let startPos = pos
        while isNumber(currentChar) {
            advance()
        }
        let endPos = input.index(input.startIndex, offsetBy: self.pos)
        return String(input[input.index(input.startIndex, offsetBy: startPos)..<endPos])
    }

    private func readIdentifier() -> String {
        let startPos = pos
        while isLetter(currentChar) {
            advance()
        }
        let endPos = input.index(input.startIndex, offsetBy: self.pos)
        return String(input[input.index(input.startIndex, offsetBy: startPos)..<endPos])
    }

    private func skipWhitespace() {
        while currentChar.isWhitespace {
            advance()
        }
    }
}


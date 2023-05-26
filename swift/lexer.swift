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
    
    /// Generates the next token from the input string, facilitating the tokenization process for subsequent parsing.
    /// - Returns: The next token in the input string.
    func nextToken() -> Token {
        skipWhitespace()
        
        if currentChar == "\0" {
            advance()
            return createToken(type: .eof, literal: "eof")
        }
        
        if isLetter(currentChar) {
            let ident = readIdentifier()
            advance()
            if let keyword = keywordMap[ident] {
                return keyword
            } else {
                return createToken(type: .ident, literal: ident)
            }
        }
        
        if isNumber(currentChar) {
            let num = readNumber()
            advance()
            return createToken(type: .int, literal: num)
        }

        let specialChars: [Character: TokenType] = [
            "{": .lSqirly,
            "}": .rSqirly,
            "(": .lParen,
            ")": .rParen,
            ",": .comma,
            ";": .semi,
            "+": .plus,
            "=": .equal
        ]

        if let tokenType = specialChars[currentChar] {
            advance()
            return createToken(type: tokenType, literal: String(currentChar))
        }
        
        advance()
        return createToken(type: .illegal, literal: String(currentChar))
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
        let index = input.index(input.startIndex, offsetBy: pos)
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
        let endPos = input.index(input.startIndex, offsetBy: pos)
        return String(input[input.index(input.startIndex, offsetBy: startPos)..<endPos])
    }

    private func readIdentifier() -> String {
        let startPos = pos
        while isLetter(currentChar) {
            advance()
        }
        let endPos = input.index(input.startIndex, offsetBy: pos)
        return String(input[input.index(input.startIndex, offsetBy: startPos)..<endPos])
    }

    private func skipWhitespace() {
        while currentChar.isWhitespace {
            advance()
        }
    }
}


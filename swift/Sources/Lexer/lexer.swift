import Foundation

struct Lexer {

    let input: String
    let keyWords: [String: Token]
    var currentIndex: String.Index

    static func from(code: String, keyWords: [String: Token] = .defaultKeyWords) -> Lexer {
        Lexer(input: code, keyWords: keyWords, currentIndex: code.startIndex)
    }

    mutating func nextToken() -> Token {

        guard currentIndex < input.endIndex else {
            return .eof
        }

        while self.input[currentIndex].isWhitespace {
            currentIndex = input.index(after: currentIndex)
        }

        let current = input[currentIndex]

        var moveSize = 1
        defer {
            // Move to the next character after we have returned this token.
            if currentIndex < input.endIndex {
                currentIndex = input.index(currentIndex, offsetBy: moveSize)
            }
        }

        switch current {
            case "+": return .plus
            case "(": return .lParen
            case ")": return .rParen
            case "{": return .lSqirly
            case "}": return .rSqirly
            case ",": return .comma
            case "=": 
                switch peekNext() {
                    case "=": 
                        moveSize = 2
                        return .equals
                    default: return .assign
                }
            case ";": return .semi
            case "!":
                switch peekNext() {
                    case "=": 
                        moveSize = 2
                        return .notEquals
                    default: return .bang
                }
            case let char where char.isValidIdentifier:
                let ident = input.identifier(from: currentIndex)
                moveSize = ident.count
                return keyWords[ident, default: .ident(ident)]
            case let char where char.isNumber:
                let numString = input.numberString(from: currentIndex)
                guard let num = Int(numString) else {
                    return .illegal(numString)
                }
                moveSize = numString.count
                return .int(num)
            default: 
                return .illegal(String(current))
        }
    }

    func peekNext() -> Character? {
        guard currentIndex < input.endIndex else {
            return nil
        }
        let nextIndex = input.index(after: currentIndex)
        return input[nextIndex]
    }
}
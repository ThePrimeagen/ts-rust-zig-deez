import Foundation

enum Token {
    case illegal(String), ident(String), int(Int), eof, comma, semi, plus, equal, lParen, rParen, lSqirly, rSqirly, function, `let`
}

extension Character {
    var isValidIdentifier: Bool {
        self.isLetter || self == "_"
    }
}

extension String {
    func numberString(from index: String.Index) -> String? {
        var endIndex = self.index(after: index)
        while self[endIndex].isNumber {
            endIndex = self.index(after: endIndex)
        }
        return String(self[index..<endIndex])
    }

    func identifier(from index: String.Index) -> String? {
        var endIndex = self.index(after: index)
        while self[endIndex].isValidIdentifier {
            endIndex = self.index(after: endIndex)
        }
        return String(self[index..<endIndex])
    }
}

struct Lexer {

    var input: String
    
    func iterator() -> AnyIterator<Token> {

        var currentIndex = self.input.startIndex

        return AnyIterator<Token> {

            while self.input[currentIndex].isWhitespace {
                currentIndex = self.input.index(after: currentIndex)
            }

            let current = self.input[currentIndex]

            defer {
                // Move to the next character after we have returned this token.
                currentIndex = self.input.index(after: currentIndex)
            }

            switch current {
                case "=": return .equal
                case "+": return .plus
                case "(": return .lParen
                case ")": return .rParen
                case "{": return .lSqirly
                case "}": return .rSqirly
                case ",": return .comma
                case ";": return .semi
                case "\0": return .eof
                case let char where char.isValidIdentifier:
                    guard let ident = self.input.identifier(from: currentIndex) else {
                        return .eof
                    }
                    currentIndex = self.input.index(currentIndex, offsetBy: ident.count)
                    switch ident {
                        case "fn": return .function
                        case "let": return .let
                        default: return .ident(ident)
                    }
                case let char where char.isNumber:
                    guard 
                        let numString = self.input.numberString(from: currentIndex),
                        let num = Int(numString) 
                    else {
                        return .eof
                    }
                    currentIndex = self.input.index(currentIndex, offsetBy: numString.count)
                    return .int(num)
                default: 
                    return .illegal(String(current))
            }

        }
    }
}

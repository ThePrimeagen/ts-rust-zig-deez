import Foundation


extension Character {
    var isValidIdentifier: Bool {
        self.isLetter || self == "_"
    }
}

extension String {
    func numberString(from index: String.Index) -> String {

        var endIndex = self.index(after: index)

        while endIndex < self.endIndex {
            if self[endIndex].isNumber == false {
                return String(self[index..<endIndex])
            }
            endIndex = self.index(after: endIndex)
        }
        return String(self[index...])
    }

    func identifier(from index: String.Index) -> String {
        var endIndex = self.index(after: index)

        while endIndex < self.endIndex {
            if self[endIndex].isValidIdentifier == false {
                return String(self[index..<endIndex])
            }
            endIndex = self.index(after: endIndex)
        }
        return String(self[index...])
    }
}

struct Lexer {

    let input: String
    var currentIndex: String.Index

    init(input: String) {
        self.input = input
        self.currentIndex = input.startIndex
    }
    
    mutating func nextToken() -> Token {

        guard currentIndex < input.endIndex else {
            return .eof
        }

        while self.input[currentIndex].isWhitespace {
            currentIndex = input.index(after: currentIndex)
        }

        let current = input[currentIndex]

        defer {
            // Move to the next character after we have returned this token.
            if currentIndex < input.endIndex {
                currentIndex = input.index(after: currentIndex)
            }
        }

        switch current {
            case "=": return .assign
            case "+": return .plus
            case "(": return .lParen
            case ")": return .rParen
            case "{": return .lSqirly
            case "}": return .rSqirly
            case ",": return .comma
            case ";": return .semi
            case let char where char.isValidIdentifier:
                let ident = input.identifier(from: currentIndex)
                currentIndex = input.index(currentIndex, offsetBy: ident.count)
                switch ident {
                    case "fn": return .function
                    case "let": return .let
                    default: return .ident(ident)
                }
            case let char where char.isNumber:
                let numString = input.numberString(from: currentIndex)
                guard let num = Int(numString) else {
                    return .illegal(numString)
                }
                currentIndex = input.index(currentIndex, offsetBy: numString.count)
                return .int(num)
            default: 
                return .illegal(String(current))
        }
    }
}

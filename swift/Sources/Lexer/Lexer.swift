extension Character {
    var isValidIdentifier: Bool {
        self.isLetter || self == "_"
    }
}

extension String {
    func numberString(from index: String.Index) -> String {
        var endIndex = self.index(after: index)
        while endIndex < self.endIndex && self[endIndex].isNumber {
            endIndex = self.index(after: endIndex)
        }
        return String(self[index..<endIndex])
    }

    func identifier(from index: String.Index) -> String {
        var endIndex = self.index(after: index)
        while endIndex < self.endIndex && self[endIndex].isValidIdentifier {
            endIndex = self.index(after: endIndex)
        }
        return String(self[index..<endIndex])
    }
}

struct Lexer {

    let input: String
    lazy var currentIndex = input.startIndex
    var currentChar: Character {
        mutating get {
            input[currentIndex]
        }
    }
    
    mutating func nextToken() -> Token {
        while currentIndex < input.endIndex && currentChar.isWhitespace {
            currentIndex = input.index(after: currentIndex)
        }
        guard currentIndex < input.endIndex else { return .eof }
        defer {
            // Move to the next character after we have returned this token.
            if currentIndex < input.endIndex {
                currentIndex = input.index(after: currentIndex)
            }
        }
        switch currentChar {
            case "=":
                let peakIndex = input.index(after: currentIndex)
                guard peakIndex != input.endIndex else { return .bang }
                switch input[peakIndex] {
                    case "=":
                        currentIndex = peakIndex
                        return .equal
                    default: return .assign
                }
            case "+": return .plus
            case "-": return .minus
            case "!":
                let peakIndex = input.index(after: currentIndex)
                guard peakIndex != input.endIndex else { return .bang }
                switch input[peakIndex] {
                    case "=":
                        currentIndex = peakIndex
                        return .notEqual
                    default: return .bang
                }
            case "/": return .slash
            case "*": return .asterisk
            case "<": return .lessThan
            case ">": return .greaterThan
            case "(": return .lParen
            case ")": return .rParen
            case "{": return .lSquirly
            case "}": return .rSquirly
            case ",": return .comma
            case ";": return .semi
            case let char where char.isValidIdentifier:
                let ident = input.identifier(from: currentIndex)
                currentIndex = input.index(currentIndex, offsetBy: ident.count - 1)
                switch ident {
                    case "fn": return .function
                    case "let": return .let
                    case "true": return .true
                    case "false": return .false
                    case "if": return .if
                    case "else": return .else
                    case "return": return .return
                    default: return .ident(ident)
                }
            case let char where char.isNumber:
                let numString = input.numberString(from: currentIndex)
                guard let num = Int(numString) else {
                    return .illegal(numString)
                }
                currentIndex = input.index(currentIndex, offsetBy: numString.count - 1)
                return .int(num)
            default: 
                return .illegal(String(currentChar))
        }
    }
}

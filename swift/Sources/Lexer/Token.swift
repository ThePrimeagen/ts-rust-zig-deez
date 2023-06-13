
enum Token: Equatable {
    case illegal(String)
    case ident(String)
    case int(Int)
    case eof
    case comma
    case semi
    case plus
    case minus
    case bang
    case slash
    case asterisk
    case lessThan
    case greaterThan
    case assign
    case equal
    case notEqual
    case lParen
    case rParen
    case lSquirly
    case rSquirly
    case function
    case `let`
    case `true`
    case `false`
    case `if`
    case `else`
    case `return`

    var literal: String {
        switch self {
            case .illegal(let literal): return literal
            case .ident(let literal): return literal
            case .int(let literal): return String(literal)
            case .eof: return "end of file"
            case .comma: return ","
            case .semi: return ";"
            case .plus: return "+"
            case .minus: return "-"
            case .bang: return "!"
            case .slash: return "/"
            case .asterisk: return "*"
            case .lessThan: return "<"
            case .greaterThan: return ">"
            case .assign: return "="
            case .equal: return "=="
            case .notEqual: return "!="
            case .lParen: return "("
            case .rParen: return ")"
            case .lSquirly: return "{"
            case .rSquirly: return "}"
            case .function: return "fn"
            case .let: return "let"
            case .true: return "true"
            case .false: return "false"
            case .if: return "if"
            case .else: return "else"
            case .return: return "return"
        }
    }
}

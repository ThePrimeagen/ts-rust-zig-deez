
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
    case asterisk
    case slash
    case equals
    case notEquals
    case assign
    case `true`
    case `false`
    case `if`
    case `else`
    case `return`
    case lParen
    case rParen
    case lSquirly
    case rSquirly
    case function
    case `let`

    var literal: String {
        switch self {
            case .illegal(let literal): return literal
            case .ident(let literal): return literal
            case .int(let literal): return String(literal)
            case .eof: return "end of file"
            case .comma: return ","
            case .semi: return ";"
            case .plus: return "+"
            case .assign: return "="
            case .lParen: return "("
            case .rParen: return ")"
            case .lSquirly: return "{"
            case .rSquirly: return "}"
            case .function: return "fn"
            case .let: return "let"
            case .minus: return "-"
            case .bang: return "!"
            case .asterisk: return "*"
            case .slash: return "/"
            case .equals: return "=="
            case .notEquals: return "!="
            case .true: return "true"
            case .false: return "false"
            case .if: return "if"
            case .else: return "else"
            case .return: return "return"
        }
    }
}

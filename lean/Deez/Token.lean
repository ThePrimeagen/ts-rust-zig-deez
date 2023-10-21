inductive Token
  | illegal
  | eof
  -- Identifiers
  | ident : String → Token
  | int : String → Token
  -- Operators
  | greaterThan
  | lessThan
  | notEqual
  | asterisk
  | assign
  | equal
  | minus
  | slash
  | plus
  | bang
  -- Delimiters
  | semicolon
  | lParen
  | rParen
  | lBrace
  | rBrace
  | comma
  -- Keywords
  | function
  | return
  | false
  | true
  | else
  | let
  | if
deriving Repr, BEq

instance : ToString Token where
  toString : Token → String
    | .illegal => "Illegal"
    | .eof => "Eof"
    -- Identifiers
    | .ident str => "Ident(" ++ str ++ ")"
    | .int n => "Int(" ++ n ++ ")"
    -- Operators
    | .greaterThan => "GreaterThan"
    | .lessThan => "LessThan"
    | .notEqual => "NotEqual"
    | .asterisk => "Asterisk"
    | .assign => "Assign"
    | .equal => "Equal"
    | .minus => "Minus"
    | .slash => "Slash"
    | .plus => "Plus"
    | .bang => "Bang"
    -- Delimiters
    | .semicolon => "Semicolon"
    | .lParen => "Lparen"
    | .rParen => "Rparen"
    | .lBrace => "Lbrace"
    | .rBrace => "Rbrace"
    | .comma => "Comma"
    -- Keywords
    | .function => "Function"
    | .return => "Return"
    | .false => "False"
    | .true => "True"
    | .else => "Else"
    | .let => "Let"
    | .if => "If"

def String.ofToken : String → Token
  | "fn" => .function
  | "return" => .return
  | "false" => .false
  | "true" => .true
  | "else" => .else
  | "let" => .let
  | "if" => .if
  | str => .ident str

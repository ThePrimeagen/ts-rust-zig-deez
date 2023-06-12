extension Dictionary where Key == String, Value == Token {
     static var `defaultKeyWords`: [String: Token] {
         [
             "fn": .function,
             "let": .let,
             "true": .true,
             "false": .false,
             "if": .if,
             "else": .else,
             "return": .return
         ]
     }
}
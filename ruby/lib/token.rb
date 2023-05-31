# frozen_string_literal: true

module Token
  Illegal = Data.define
  Eof = Data.define

  Ident = Data.define(:ident)
  Int = Data.define(:int)
  Assign = Data.define
  Plus = Data.define
  Minus = Data.define
  Bang = Data.define
  Asterisk = Data.define
  Slash = Data.define

  NotEq = Data.define
  Equal = Data.define

  Lt = Data.define
  Gt = Data.define

  Comma = Data.define
  Semicolon = Data.define
  LParen = Data.define
  RParen = Data.define
  LSquirly = Data.define
  RSquirly = Data.define

  Function = Data.define
  Let = Data.define
  True = Data.define
  False = Data.define
  If = Data.define
  Else = Data.define
  Return = Data.define
end

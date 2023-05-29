# frozen_string_literal: true

module Token
  Illegal = Data.new
  Eof = Data.new

  Ident = Data.new
  Int = Data.new
  Assign = Data.new
  Plus = Data.new
  Comma = Data.new
  Semicolon = Data.new
  LParen = Data.new
  RParen = Data.new
  LSquirly = Data.new
  RSquirly = Data.new

  Function = Data.new
  Let = Data.new
end

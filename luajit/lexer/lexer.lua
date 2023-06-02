local Token = require("token")
local ffi = require("ffi")

ffi.cdef[[
  typedef struct {
    const char *input; 
    char ch;
    size_t len;
    size_t read_position;
    size_t position;
  } Lexer;
]]
local C = ffi.C

local lexer = {}

lexer.init = function (input)

  assert(type(input) == "string")

  local lexStruct = ffi.new("Lexer")
  lexStruct.input = input
  lexStruct.ch = 0
  lexStruct.len = #input
  lexStruct.read_position = 0
  lexStruct.position = 0

  local l = {
    input = input,
    lexDat = lexStruct
  }

  l.next_token = function (self)
    self:skip_whitespace()

    local as_string = string.char(self.lexDat.ch)

    local tok = Token(Token.type.illegal)

    local keywords = {
      ['let'] = Token(Token.type.let),
      ['fn'] = Token(Token.type.fn)
    }

    local switch = {
      ['{'] = Token(Token.type.lsquirly),
      ['}'] = Token(Token.type.rsquirly),
      ['('] = Token(Token.type.lparen),
      [')'] = Token(Token.type.rparen),
      [','] = Token(Token.type.comma),
      [';'] = Token(Token.type.semicolon),
      ['+'] = Token(Token.type.plus),
      ['='] = Token(Token.type.equal),
      ['\0'] = Token(Token.type.eof),
    }

    if switch[as_string] then 
      tok = switch[as_string]
    end

    if as_string:match("[a-zA-Z_]") ~= nil then
      local ident = self:read_identifier()

      if keywords[ffi.string(ident)] then
        return(keywords[ffi.string(ident)])
      end

      return Token(Token.type.ident, ffi.string(ident))
    end

    if as_string:match("[0-9]") ~= nil then
      local int = self:read_int()
      return Token(Token.type.int, ffi.string(int))
    end

    self:read_char()
    return tok
  end

  l.read_char = function (self)
    local self = self.lexDat
    if self.read_position >= self.len then
      self.ch = 0
    else
      self.ch = self.input[self.read_position]
    end

    self.position = self.read_position
    self.read_position = self.read_position + 1
  end

  l.read_identifier = function (self)
    local position = self.lexDat.position

    while string.char(self.lexDat.ch):match("([a-zA-Z_])") ~= nil do
      self:read_char()
    end

    local out_str = ffi.new("char[?]", (self.lexDat.position - position)+1)
    ffi.copy(out_str, self.lexDat.input+position, (self.lexDat.position - position))

    return out_str
  end

  l.read_int = function (self)
    local position = self.lexDat.position

    while string.char(self.lexDat.ch):match("[0-9]") do
      self:read_char()
    end

    local out_str = ffi.new("char[?]", (self.lexDat.position - position)+1)
    ffi.copy(out_str, self.lexDat.input+position, (self.lexDat.position - position))

    return out_str
  end

  l.skip_whitespace = function (self)
    while string.char(self.lexDat.ch):match("[ \n]") do
      self:read_char()
    end
  end

  l:read_char()

  return l;
end

setmetatable(lexer,
{
  __call = function(...)
    local args = {...}
    ---@type string
    local input = args[2]
    return lexer.init(input)
  end
})

return lexer
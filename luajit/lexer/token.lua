local ffi = require("ffi")

ffi.cdef[[
  typedef enum {
    TokenTypeIllegal,
    TokenTypeEof,
    TokenTypeIdent,
    TokenTypeInt,
    TokenTypeEqual,
    TokenTypePlus,
    TokenTypeComma,
    TokenTypeSemicolon,
    TokenTypeLParen,
    TokenTypeRParen,
    TokenTypeLSquirly,
    TokenTypeRSquirly,
    TokenTypeFunction,
    TokenTypeLet,
  } TokenType;

  typedef struct {
    TokenType type;
    const char *literal; 
  } Token;
]]
local C = ffi.C

local token = {}

---@class token_type
token.type = {
  illegal = C.TokenTypeIllegal,
  eof = C.TokenTypeEof,
  ident = C.TokenTypeIdent,
  int = C.TokenTypeInt,
  equal = C.TokenTypeEqual,
  plus = C.TokenTypePlus,
  comma = C.TokenTypeComma,
  semicolon = C.TokenTypeSemicolon,
  lparen = C.TokenTypeLParen,
  rparen = C.TokenTypeRParen,
  lsquirly = C.TokenTypeLSquirly,
  rsquirly = C.TokenTypeRSquirly,
  fn = C.TokenTypeFunction,
  let = C.TokenTypeLet,
}

token.inv_type = {}

for k, v in pairs(token.type) do
  token.inv_type[tonumber(v)] = k
end

local token_mt

local mt = {
  __index = function (type, accessor)
    local fn = {}
    fn.literal_str = function()
      if type.literal ~= nil then
        return ffi.string(type.literal)
      end
      return ""
    end

    return fn[accessor]()
  end,
}
token_mt = ffi.metatype("Token", mt)

setmetatable(token,
{
  __call = function(...)
    local args = {...}
    ---@type token_type
    local token_type = args[2]
    ---@type string
    local literal = args[3]
    assert(type(literal) == "string" or not literal)
    return token_mt(token_type, literal)
  end
})

return token
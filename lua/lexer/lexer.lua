#!/usr/bin/env luajit

local M = {}

M.token_t = {
  ASSIGN = 0,
  COMMA = 1,
  EOF = 2,
  FUNCTION = 3,
  IDENTIFIER = 4,
  ILLEGAL = 5,
  INTEGER = 6,
  LET = 7,
  LPAREN = 8,
  LSQUIRLY = 9,
  PLUS = 10,
  RPAREN = 11,
  RSQUIRLY = 12,
  SEMICOLON = 13,
}

M.lexer = function(str)
  if str == nil or str == "" then
    error("Empty Input Not Permitted", 1)
  end
  M.input = str
  M.pos = 1
  M.curr_byte = str:byte(1)
end

local read_next_byte = function()
  if M.pos >= #M.input then
    M.curr_byte = 0
    return
  end

  M.pos = M.pos + 1
  M.curr_byte = M.input:byte(M.pos)
end

local is_letter = function()
  return (97 <= M.curr_byte and M.curr_byte <= 122)
    or (65 <= M.curr_byte and M.curr_byte <= 90)
    or M.curr_byte == 95
end

local is_digit = function()
  return (48 <= M.curr_byte and M.curr_byte <= 57)
end

local get_number = function()
  local start = M.pos
  repeat
    read_next_byte()
  until not is_digit()
  return M.input:sub(start, M.pos - 1)
end

local get_identifier = function()
  local start = M.pos
  repeat
    read_next_byte()
  until not is_letter()
  return M.input:sub(start, M.pos - 1)
end

local skip_whitespace = function()
  while M.curr_byte == 10 or M.curr_byte == 9 or M.curr_byte == 13 or M.curr_byte == 32 do
    read_next_byte()
  end
end

local literal_to_token = {
  ["fn"] = M.token_t.FUNCTION,
  ["let"] = M.token_t.LET,
}

M.next_token = function()
  skip_whitespace()
  local token = { type = M.token_t.ILLEGAL, literal = string.char(M.curr_byte) }

  if token.literal == "=" then
    token.type = M.token_t.ASSIGN
  elseif token.literal == "+" then
    token.type = M.token_t.PLUS
  elseif token.literal == "\0" then
    token.type = M.token_t.EOF
    token.literal = "eof"
  elseif token.literal == ";" then
    token.type = M.token_t.SEMICOLON
  elseif token.literal == "," then
    token.type = M.token_t.COMMA
  elseif token.literal == "(" then
    token.type = M.token_t.LPAREN
  elseif token.literal == ")" then
    token.type = M.token_t.RPAREN
  elseif token.literal == "{" then
    token.type = M.token_t.LSQUIRLY
  elseif token.literal == "}" then
    token.type = M.token_t.RSQUIRLY
  end

  if is_digit() then
    return { type = M.token_t.INTEGER, literal = get_number() }
  end

  if is_letter() then
    local ident = get_identifier()
    local tok = literal_to_token[ident]
    if tok then
      return { type = tok, literal = ident }
    end
    return { type = M.token_t.IDENTIFIER, literal = ident }
  end

  read_next_byte()
  return token
end

return M

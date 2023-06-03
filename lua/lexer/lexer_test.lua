#!/usr/bin/env luajit

local lex = require ".lexer"

local test_single_char_tokens = function()
  local input = "=+(){},;"
  lex.lexer(input)
  local expected = {
    lex.token_t.ASSIGN,
    lex.token_t.PLUS,
    lex.token_t.LPAREN,
    lex.token_t.RPAREN,
    lex.token_t.LSQUIRLY,
    lex.token_t.RSQUIRLY,
    lex.token_t.COMMA,
    lex.token_t.SEMICOLON,
    lex.token_t.EOF,
  }
  for _, t in ipairs(expected) do
    local g = lex.next_token().type
    if g ~= t then
      error(t .. "Not matching as expected", 1)
    end
  end
end

local test_multiple_char_tokens = function()
  local input = "let five = 5;\n"
    .. "let ten = 10;\n"
    .. "let add = fn(x, y) {\n"
    .. "     x + y;\n"
    .. "};\n"
    .. "let result = add(five, ten);"
  lex.lexer(input)
  local expected = {
    { lex.token_t.LET, "let" },
    { lex.token_t.IDENTIFIER, "five" },
    { lex.token_t.ASSIGN, "=" },
    { lex.token_t.INTEGER, "5" },
    { lex.token_t.SEMICOLON, ";" },
    { lex.token_t.LET, "let" },
    { lex.token_t.IDENTIFIER, "ten" },
    { lex.token_t.ASSIGN, "=" },
    { lex.token_t.INTEGER, "10" },
    { lex.token_t.SEMICOLON, ";" },
    { lex.token_t.LET, "let" },
    { lex.token_t.IDENTIFIER, "add" },
    { lex.token_t.ASSIGN, "=" },
    { lex.token_t.FUNCTION, "fn" },
    { lex.token_t.LPAREN, "(" },
    { lex.token_t.IDENTIFIER, "x" },
    { lex.token_t.COMMA, "," },
    { lex.token_t.IDENTIFIER, "y" },
    { lex.token_t.RPAREN, ")" },
    { lex.token_t.LSQUIRLY, "{" },
    { lex.token_t.IDENTIFIER, "x" },
    { lex.token_t.PLUS, "+" },
    { lex.token_t.IDENTIFIER, "y" },
    { lex.token_t.SEMICOLON, ";" },
    { lex.token_t.RSQUIRLY, "}" },
    { lex.token_t.SEMICOLON, ";" },
    { lex.token_t.LET, "let" },
    { lex.token_t.IDENTIFIER, "result" },
    { lex.token_t.ASSIGN, "=" },
    { lex.token_t.IDENTIFIER, "add" },
    { lex.token_t.LPAREN, "(" },
    { lex.token_t.IDENTIFIER, "five" },
    { lex.token_t.COMMA, "," },
    { lex.token_t.IDENTIFIER, "ten" },
    { lex.token_t.RPAREN, ")" },
    { lex.token_t.SEMICOLON, ";" },
    { lex.token_t.EOF, "eof" },
  }
  for _, t in ipairs(expected) do
    local g = lex.next_token()
    if g.literal ~= t[2] or g.type ~= t[1] then
      error(t[2] .. "Not matching as expected", 1)
    end
  end
end

test_single_char_tokens()
test_multiple_char_tokens()

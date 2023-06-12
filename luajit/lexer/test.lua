local Lexer = require("lexer")
local Token = require("token")

local test = {}

test["Lexer"] = function ()
    local input = "=+(){},;"
    local lex = Lexer(input)

    local tokens = {
        Token(Token.type.equal),
        Token(Token.type.plus),
        Token(Token.type.lparen),
        Token(Token.type.rparen),
        Token(Token.type.lsquirly),
        Token(Token.type.rsquirly),
        Token(Token.type.comma),
        Token(Token.type.semicolon),
        Token(Token.type.eof),
    }

    for ind, exp_tok in ipairs(tokens) do
        local fnd_tok = lex:next_token();
        assert(exp_tok.type == fnd_tok.type)
    end
end

test["Lexer - Full"] = function ()
    
    local input = [[let five = 5;
    let ten = 10;
    let add = fn(x, y) {
        x + y;
    };
    let result = add(five, ten);]]

    local lex = Lexer(input)

    local tokens = {
        Token(Token.type.let),
        Token(Token.type.ident, "five"),
        Token(Token.type.equal),
        Token(Token.type.int, "5"),
        Token(Token.type.semicolon),
        Token(Token.type.let),
        Token(Token.type.ident, "ten"),
        Token(Token.type.equal),
        Token(Token.type.int, "10"),
        Token(Token.type.semicolon),
        Token(Token.type.let),
        Token(Token.type.ident, "add"),
        Token(Token.type.equal),
        Token(Token.type.fn),
        Token(Token.type.lparen),
        Token(Token.type.ident, "x"),
        Token(Token.type.comma),
        Token(Token.type.ident, "y"),
        Token(Token.type.rparen),
        Token(Token.type.lsquirly),
        Token(Token.type.ident, "x"),
        Token(Token.type.plus),
        Token(Token.type.ident, "y"),
        Token(Token.type.semicolon),
        Token(Token.type.rsquirly),
        Token(Token.type.semicolon),
        Token(Token.type.let),
        Token(Token.type.ident, "result"),
        Token(Token.type.equal),
        Token(Token.type.ident, "add"),
        Token(Token.type.lparen),
        Token(Token.type.ident, "five"),
        Token(Token.type.comma),
        Token(Token.type.ident, "ten"),
        Token(Token.type.rparen),
        Token(Token.type.semicolon),
        Token(Token.type.eof)
    }

    for ind, exp_tok in ipairs(tokens) do
        local fnd_tok = lex:next_token()
        assert(exp_tok.type == fnd_tok.type and exp_tok.literal_str == fnd_tok.literal_str)
    end

end

local count = 0
for _, fn in pairs(test) do
    fn()
    count=count+1
end

print(count, "Succeeded.")
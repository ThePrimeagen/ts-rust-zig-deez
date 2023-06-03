module Tokens

abstract type TokenType end

struct ILLEGAL <: TokenType end
struct EOF <: TokenType end
struct IDENT <: TokenType end
struct INT <: TokenType end
struct ASSIGN <: TokenType end
struct PLUS <: TokenType end
struct COMMA <: TokenType end
struct SEMICOLON <: TokenType end
struct LPAREN <: TokenType end
struct RPAREN <: TokenType end
struct LBRACE <: TokenType end
struct RBRACE <: TokenType end

struct Token
    Type::TokenType
    Literal::String
end

export ILLEGAL, EOF, IDENT, INT, ASSIGN, PLUS, COMMA, SEMICOLON, LPAREN, RPAREN, LBRACE, RBRACE, Token

end # module Tokens

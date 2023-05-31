require 'rspec'
require_relative '../src/lexer'

describe Tokenizer do
  it 'Test Tokenizer' do
    input = %q(
        let five = 5;
        let ten = 10;

        let add = fn(x, y) {
            x + y;
        };

        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
    )
    lexer = Tokenizer.new(input)

    tokens = [
        { type: TokenType::Let, literal: 'let' },
        { type: TokenType::Ident, literal: 'five' },
        { type: TokenType::Equal, literal: '=' },
        { type: TokenType::Int, literal: '5' },
        { type: TokenType::Semicolon, literal: ';' },
        { type: TokenType::Let, literal: 'let' },
        { type: TokenType::Ident, literal: 'ten' },
        { type: TokenType::Equal, literal: '=' },
        { type: TokenType::Int, literal: '10' },
        { type: TokenType::Semicolon, literal: ';' },
        { type: TokenType::Let, literal: 'let' },
        { type: TokenType::Ident, literal: 'add' },
        { type: TokenType::Equal, literal: '=' },
        { type: TokenType::Function, literal: 'fn' },
        { type: TokenType::LParen, literal: '(' },
        { type: TokenType::Ident, literal: 'x' },
        { type: TokenType::Comma, literal: ',' },
        { type: TokenType::Ident, literal: 'y' },
        { type: TokenType::RParen, literal: ')' },
        { type: TokenType::LSquirly, literal: '{' },
        { type: TokenType::Ident, literal: 'x' },
        { type: TokenType::Plus, literal: '+' },
        { type: TokenType::Ident, literal: 'y' },
        { type: TokenType::Semicolon, literal: ';' },
        { type: TokenType::RSquirly, literal: '}' },
        { type: TokenType::Semicolon, literal: ';' },
        { type: TokenType::Let, literal: 'let' },
        { type: TokenType::Ident, literal: 'result' },
        { type: TokenType::Equal, literal: '=' },
        { type: TokenType::Ident, literal: 'add' },
        { type: TokenType::LParen, literal: '(' },
        { type: TokenType::Ident, literal: 'five' },
        { type: TokenType::Comma, literal: ',' },
        { type: TokenType::Ident, literal: 'ten' },
        { type: TokenType::RParen, literal: ')' },
        { type: TokenType::Semicolon, literal: ';' },
        { type: TokenType::Bang, literal: '!' },
        { type: TokenType::Minus, literal: '-' },
        { type: TokenType::Slash, literal: '/' },
        { type: TokenType::Asterisk, literal: '*' },
        { type: TokenType::Int, literal: '5' },
        { type: TokenType::Semicolon, literal: ';' },
        { type: TokenType::Int, literal: '5' },
        { type: TokenType::LT, literal: '<' },
        { type: TokenType::Int, literal: '10' },
        { type: TokenType::GT, literal: '>' },
        { type: TokenType::Int, literal: '5' },
        { type: TokenType::Semicolon, literal: ';' },
        { type: TokenType::If, literal: 'if' },
        { type: TokenType::LParen, literal: '(' },
        { type: TokenType::Int, literal: '5' },
        { type: TokenType::LT, literal: '<' },
        { type: TokenType::Int, literal: '10' },
        { type: TokenType::RParen, literal: ')' },
        { type: TokenType::LSquirly, literal: '{' },
        { type: TokenType::Return, literal: 'return' },
        { type: TokenType::True, literal: 'true' },
        { type: TokenType::Semicolon, literal: ';' },
        { type: TokenType::RSquirly, literal: '}' },
        { type: TokenType::Else, literal: 'else' },
        { type: TokenType::LSquirly, literal: '{' },
        { type: TokenType::Return, literal: 'return' },
        { type: TokenType::False, literal: 'false' },
        { type: TokenType::Semicolon, literal: ';' },
        { type: TokenType::RSquirly, literal: '}' },
        { type: TokenType::Int, literal: '10' },
        { type: TokenType::EQ, literal: '==' },
        { type: TokenType::Int, literal: '10' },
        { type: TokenType::Semicolon, literal: ';' },
        { type: TokenType::Int, literal: '10' },
        { type: TokenType::NotEQ, literal: '!=' },
        { type: TokenType::Int, literal: '9' },
    ]

    tokens.each do |token|
        expect(lexer.next_token).to eq(token)
    end
   end
end
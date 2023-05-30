require 'rspec'
require_relative 'lexer'

describe Tokenizer do
  it 'tests getNextToken()' do
    input = '=+(){},;'
    tokens = [
      TokenType::Equal,
      TokenType::Plus,
      TokenType::LParen,
      TokenType::RParen,
      TokenType::LSquirly,
      TokenType::RSquirly,
      TokenType::Comma,
      TokenType::Semicolon
    ]

    lexer = Tokenizer.new(input)

    tokens.each do |token|
      expect(lexer.next_token[:type]).to eq(token)
    end
  end

  it 'test getNextToken() complete' do
    input = %q(
        let five = 5;
        let ten = 10;

        let add = fn(x, y) {
            x + y;
        };

        let result = add(five, ten);
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
        { type: TokenType::Eof, literal: '' }
    ]

    tokens.each do |token|
        expect(lexer.next_token).to eq(token)
    end
   end
end
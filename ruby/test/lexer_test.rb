# frozen_string_literal: true

require 'minitest/autorun'
require_relative '../lib/lexer'


class LexerTest < Minitest::Test
  def test_lex_one_line
    input = "=+(){},;"

    tests = [
      Token::Assign,
      Token::Plus,
      Token::LParen,
      Token::RParen,
      Token::LSquirly,
      Token::RSquirly,
      Token::Comma,
      Token::Semicolon,
      Token::Eof
    ]

    l = Lexer.new(input)

    tests.each do |t|
      tok = l.next_token

      puts "expected #{t} got #{tok}"

      assert_equal t, tok
    end
  end

  def test_lex_without_combo_symbols
    input = "let five = 5;
    let ten = 10;

    let add = fn(x, y) {
      x + y;
    };"

    tests = [
      Token::Let,
      Token::Ident.new("five"),
      Token::Assign,
      Token::Int.new("5"),
      Token::Semicolon,
      Token::Let,
      Token::Ident.new("ten"),
      Token::Assign,
      Token::Int.new("10"),
      Token::Semicolon,
      Token::Let,
      Token::Ident.new("add"),
      Token::Assign,
      Token::Function,
      Token::LParen,
      Token::Ident.new("x"),
      Token::Comma,
      Token::Ident.new("y"),
      Token::RParen,
      Token::LSquirly,
      Token::Ident.new("x"),
      Token::Plus,
      Token::Ident.new("y"),
      Token::Semicolon,
      Token::RSquirly,
      Token::Semicolon
    ]

    l = Lexer.new(input)

    tests.each do |t|
      tok = l.next_token

      puts "expected #{t} got #{tok}"

      assert_equal t, tok
    end
  end

  def test_lex_complete
    input = "let five = 5;
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
    10 != 9;"

    tests = [
      Token::Let,
      Token::Ident.new("five"),
      Token::Assign,
      Token::Int.new("5"),
      Token::Semicolon,
      Token::Let,
      Token::Ident.new("ten"),
      Token::Assign,
      Token::Int.new("10"),
      Token::Semicolon,
      Token::Let,
      Token::Ident.new("add"),
      Token::Assign,
      Token::Function,
      Token::LParen,
      Token::Ident.new("x"),
      Token::Comma,
      Token::Ident.new("y"),
      Token::RParen,
      Token::LSquirly,
      Token::Ident.new("x"),
      Token::Plus,
      Token::Ident.new("y"),
      Token::Semicolon,
      Token::RSquirly,
      Token::Semicolon,
      Token::Let,
      Token::Ident.new("result"),
      Token::Assign,
      Token::Ident.new("add"),
      Token::LParen,
      Token::Ident.new("five"),
      Token::Comma,
      Token::Ident.new("ten"),
      Token::RParen,
      Token::Semicolon,
      Token::Bang,
      Token::Minus,
      Token::Slash,
      Token::Asterisk,
      Token::Int.new("5"),
      Token::Semicolon,
      Token::Int.new("5"),
      Token::Lt,
      Token::Int.new("10"),
      Token::Gt,
      Token::Int.new("5"),
      Token::Semicolon,
      Token::If,
      Token::LParen,
      Token::Int.new("5"),
      Token::Lt,
      Token::Int.new("10"),
      Token::RParen,
      Token::LSquirly,
      Token::Return,
      Token::True,
      Token::Semicolon,
      Token::RSquirly,
      Token::Else,
      Token::LSquirly,
      Token::Return,
      Token::False,
      Token::Semicolon,
      Token::RSquirly,
      Token::Int.new("10"),
      Token::Equal,
      Token::Int.new("10"),
      Token::Semicolon,
      Token::Int.new("10"),
      Token::NotEq,
      Token::Int.new("9"),
      Token::Semicolon,
      Token::Eof
    ]

    l = Lexer.new(input)

    tests.each do |t|
      tok = l.next_token

      puts "expected #{t} got #{tok}"

      assert_equal t, tok
    end
  end
end

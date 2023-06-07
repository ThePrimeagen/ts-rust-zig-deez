require "./spec_helper"

describe Lexer do
  it "parses random tokens" do
    lexer = Lexer.new("=+(){},;")
    tokens = {
      Token.new(:assign),
      Token.new(:plus),
      Token.new(:left_paren),
      Token.new(:right_paren),
      Token.new(:left_squirly),
      Token.new(:right_squirly),
      Token.new(:comma),
      Token.new(:semicolon),
      Token.new(:eof),
    }

    tokens.each do |token|
      lexer.next_token.should eq token
    end
  end

  it "parses tokens fully" do
    lexer = Lexer.new <<-MONKEY
      let five = 5;
      let ten = 10;
      let add = fn(x, y) {
        x + y;
      };
      let result = add(five, ten);
      MONKEY

    tokens = {
      Token.new(:let),
      Token.new(:ident, "five"),
      Token.new(:assign),
      Token.new(:integer, "5"),
      Token.new(:semicolon),
      Token.new(:let),
      Token.new(:ident, "ten"),
      Token.new(:assign),
      Token.new(:integer, "10"),
      Token.new(:semicolon),
      Token.new(:let),
      Token.new(:ident, "add"),
      Token.new(:assign),
      Token.new(:function),
      Token.new(:left_paren),
      Token.new(:ident, "x"),
      Token.new(:comma),
      Token.new(:ident, "y"),
      Token.new(:right_paren),
      Token.new(:left_squirly),
      Token.new(:ident, "x"),
      Token.new(:plus),
      Token.new(:ident, "y"),
      Token.new(:semicolon),
      Token.new(:right_squirly),
      Token.new(:semicolon),
      Token.new(:let),
      Token.new(:ident, "result"),
      Token.new(:assign),
      Token.new(:ident, "add"),
      Token.new(:left_paren),
      Token.new(:ident, "five"),
      Token.new(:comma),
      Token.new(:ident, "ten"),
      Token.new(:right_paren),
      Token.new(:semicolon),
      Token.new(:eof),
    }

    tokens.each do |token|
      lexer.next_token.should eq token
    end
  end
end

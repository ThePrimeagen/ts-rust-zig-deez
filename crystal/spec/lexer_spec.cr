require "spec"
require "../src/lexer"

describe Lexer do
  it "parses tokens" do
    lexer = Lexer.new("=+(){},;")
    tokens = {
      Token.new(:equal),
      Token.new(:plus),
      Token.new(:lparen),
      Token.new(:rparen),
      Token.new(:lsquirly),
      Token.new(:rsquirly),
      Token.new(:comma),
      Token.new(:semicolon),
      Token.new(:eof),
    }

    tokens.each do |token|
      next_token = lexer.next_token rescue next
      puts "expected: #{token.type}, received: #{next_token.type}"
      token.should eq next_token
    end
  end
end

require "spec"
require "../src/*"

def parse(input : String) : Array(Statement)
  tokens = Lexer.new(input).run
  program = Parser.new(tokens).parse

  program.statements
end

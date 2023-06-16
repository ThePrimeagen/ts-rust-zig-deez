# frozen_string_literal: true

require_relative "./lexer"

class Repl
  def start
    trap('INT') do
      puts "\nGoodbye!"
      exit
    end

    loop do
      print '>> '
      input = gets.chomp

      l = Lexer.new(input)

      loop do
        tok = l.next_token

        break if tok == Token::Eof

        puts tok.inspect
      end
    end
  end
end

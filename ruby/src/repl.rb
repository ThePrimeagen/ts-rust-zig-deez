require 'readline'
require_relative 'lexer'

class Repl
  PROMPT = '>> '

  def start
    trap('INT') do
      puts "\nGoodbye!"
      exit
    end

    loop do
      line = Readline.readline(PROMPT, true)
      break if line.nil? || line.empty?

      l = Tokenizer.new(line)
      loop do
        tok = l.next_token
        break if tok[:type] == TokenType::Eof

        puts tok.inspect
      end
    end
  end
end
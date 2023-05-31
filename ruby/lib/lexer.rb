# frozen_string_literal: true

require_relative './token'

class String
  def ascii_whitespace?
    self == ' ' || self == "\t" || self == "\n" || self == "\r"
  end

  def ascii_alphabetic?
    case self
    when 'a'..'z', 'A'..'Z', '_'
      true
    else
      false
    end
  end

  def ascii_digit?
    Integer(self) rescue false
  end
end

class Lexer
  def initialize(input)
    @input = input
    @position = 0
    @read_position = 0
    @ch = 0

    read_char
  end

  def next_token
    skip_whitespace unless @ch.nil?

    tok = case @ch
          when '='
            if peek_char == '='
              read_char
              Token::Equal
            else
              Token::Assign
            end
          when '+'
            Token::Plus
          when '-'
            Token::Minus
          when '!'
            if peek_char == '='
              read_char
              Token::NotEq
            else
              Token::Bang
            end
          when '*'
            Token::Asterisk
          when '/'
            Token::Slash
          when '<'
            Token::Lt
          when '>'
            Token::Gt
          when ';'
            Token::Semicolon
          when '('
            Token::LParen
          when ')'
            Token::RParen
          when ','
            Token::Comma
          when '{'
            Token::LSquirly
          when '}'
            Token::RSquirly
          when 'a'..'z', 'A'..'Z', '_'
            ident = read_ident

            return case ident
                   when 'fn'
                     Token::Function
                   when 'let'
                     Token::Let
                   when 'true'
                     Token::True
                   when 'false'
                     Token::False
                   when 'if'
                     Token::If
                   when 'else'
                     Token::Else
                   when 'return'
                     Token::Return
                   else
                     Token::Ident.new(ident)
                   end
          when '1'..'9'
            return Token::Int.new(read_int)
          when nil
            Token::Eof
          else
            Token::Illegal
          end

    read_char
    tok
  end

  private

  def read_char
    @ch = if @read_position > @input.length
            nil
          else
            @input[@read_position]
          end
    @position = @read_position
    @read_position += 1
  end

  def peek_char
    if @read_position > @input.length
      nil
    else
      @input[@read_position]
    end
  end

  def read_ident
    pos = @position

    read_char while @ch.ascii_alphabetic?

    @input[pos...@position]
  end

  def read_int
    pos = @position

    read_char while @ch.ascii_digit?

    @input[pos...@position]
  end

  def skip_whitespace
    read_char while @ch.ascii_whitespace?
  end
end

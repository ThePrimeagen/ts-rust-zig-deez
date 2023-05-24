struct Token
  enum Type
    Ident
    Integer
    Illegal
    Eof
    Equal
    Plus
    Comma
    Semicolon
    Lparen
    Rparen
    Lsquirly
    Rsquirly
    Function
    Let
  end

  getter type : Type
  getter value : String?

  def initialize(@type, @value = nil)
  end

  def ==(other : Token) : Bool
    @type == other.type
  end
end

class Lexer
  @position : Int32
  @read_position : Int32
  @input : Array(Char)
  @char : Char

  def initialize(input : String)
    @position = 0
    @read_position = 0
    @input = input.chars
    @char = '\0'

    read_char
  end

  def next_token : Token
    token = case @char
            when '{'  then Token.new :lsquirly
            when '}'  then Token.new :rsquirly
            when '('  then Token.new :lparen
            when ')'  then Token.new :rparen
            when ','  then Token.new :comma
            when ';'  then Token.new :semicolon
            when '+'  then Token.new :plus
            when '='  then Token.new :equal
            when '\0' then Token.new :eof
            else           raise "this needs implementing..."
            end
    read_char

    token
  end

  private def read_char : Nil
    if @read_position >= @input.size
      @char = '\0'
    else
      @char = @input[@read_position]
    end

    @position = @read_position
    @read_position += 1
  end
end

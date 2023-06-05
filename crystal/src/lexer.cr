require "string_pool"

class Lexer
  @reader : Char::Reader
  @pool : StringPool

  def initialize(input : String)
    @reader = Char::Reader.new input
    @pool = StringPool.new
  end

  def run : Array(Token)
    tokens = [] of Token

    loop do
      token = next_token
      tokens << token
      break if token.type.eof?
    end

    tokens
  end

  def next_token : Token
    type : Token::Type
    value : String? = nil

    case current_char
    when '\0'
      type = :eof
    when ' ', '\t', '\r', '\n'
      if value = skip_whitespace
        type = :illegal
      else
        return next_token
      end
    when '='
      if next_char == '='
        type = :equal
      else
        type = :assign
      end
    when '!'
      if next_char == '='
        type = :not_equal
      else
        type = :bang
      end
    when '+'
      next_char
      type = :plus
    when '-'
      next_char
      type = :minus
    when '*'
      next_char
      type = :asterisk
    when '/'
      next_char
      type = :slash
    when '>'
      next_char
      type = :greater_than
    when '<'
      next_char
      type = :less_than
    when ','
      next_char
      type = :comma
    when ';'
      next_char
      type = :semicolon
    when '('
      next_char
      type = :left_paren
    when ')'
      next_char
      type = :right_paren
    when '{'
      next_char
      type = :left_squirly
    when '}'
      next_char
      type = :right_squirly
    when 'f'
      start = current_pos
      case next_char
      when 'n'
        next_char
        type = :function
      when 'a'
        if next_sequence?('l', 's', 'e') && check_char_bounds
          type = :false
        else
          type = :ident
          value = read_ident_from start
        end
      else
        type = :ident
        value = read_ident_from start
      end
    when 'l'
      start = current_pos
      if next_char == 'e' && next_char == 't' && check_char_bounds
        type = :let
      else
        type = :ident
        value = read_ident_from start - 1
      end
    when 't'
      start = current_pos
      if next_sequence?('r', 'u', 'e') && check_char_bounds
        type = :true
      else
        type = :ident
        value = read_ident_from start
      end
    when 'i'
      if next_char == 'f' && check_char_bounds
        type = :if
      else
        type = :ident
        value = read_ident_from(current_pos - 1)
      end
    when 'e'
      start = current_pos
      if next_sequence?('l', 's', 'e') && check_char_bounds
        type = :else
      else
        type = :ident
        value = read_ident_from start
      end
    when 'r'
      start = current_pos
      if next_sequence?('e', 't', 'u', 'r', 'n') && check_char_bounds
        type = :return
      else
        type = :ident
        value = read_ident_from start
      end
    when .ascii_letter?, '_'
      type = :ident
      value = read_ident_from current_pos
    when .ascii_number?
      type = :integer
      value = read_integer
    else
      type = :illegal
      value = "illegal token #{@reader.current_char.inspect}"
      next_char
    end

    Token.new type, value
  end

  private def skip_whitespace : String?
    loop do
      case next_char
      when ' ', '\t', '\n'
        next
      when '\r'
        return "expected '\\n' after '\\r" unless next_char == '\n'
      else
        break
      end
    end
  end

  private def read_ident_from(start : Int32) : String
    while current_char.ascii_letter? || current_char == '_'
      next_char
    end

    slice = Slice.new(@reader.string.to_unsafe + start, current_pos - start)
    @pool.get slice
  end

  private def read_integer : String
    start = current_pos
    while current_char.ascii_number?
      next_char
    end

    slice = Slice.new(@reader.string.to_unsafe + start, current_pos - start)
    @pool.get slice
  end

  private def current_char : Char
    @reader.current_char
  end

  private def current_pos : Int32
    @reader.pos
  end

  private def next_char : Char
    @reader.next_char
  end

  private def next_sequence?(*chars : Char) : Bool
    chars.all? { |char| next_char == char }
  end

  private def check_char_bounds : Bool
    next_char
    !current_char.ascii_letter? && current_char != '_'
  end
end

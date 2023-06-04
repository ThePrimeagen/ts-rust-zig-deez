struct Token
  enum Type
    Eof
    Illegal

    # identifiers
    Ident
    Integer

    # operators
    Assign
    Equal
    NotEqual
    Plus
    Minus
    Bang
    Asterisk
    Slash
    GreaterThan
    LessThan

    # delimiters
    Comma
    Semicolon
    LeftParen
    RightParen
    LeftSquirly
    RightSquirly

    # keywords
    Function
    Let
    True
    False
    If
    Else
    Return
  end

  getter type : Type
  @value : String?

  def initialize(@type, @value = nil)
  end

  def value : String
    @value.as(String)
  end

  def ==(other : Token) : Bool
    @type == other.type
  end
end

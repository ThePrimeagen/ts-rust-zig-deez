class Parser
  enum Precedence
    Lowest
    Equals
    LessGreater
    Sum
    Product
    Prefix
    Call

    def self.from(type : Token::Type)
      case type
      when .equal?, .not_equal?
        Precedence::Equals
      when .less_than?, .greater_than?
        Precedence::LessGreater
      when .plus?, .minus?
        Precedence::Sum
      when .slash?, .asterisk?
        Precedence::Product
      when .left_paren?
        Precedence::Call
      else
        Precedence::Lowest
      end
    end
  end

  @tokens : Array(Token)
  @pos : Int32

  def initialize(@tokens)
    @pos = -1
  end

  def parse : Program
    statements = [] of Statement

    loop do
      statement = parse_statement next_token
      break if statement.nil?
      statements << statement
    end

    Program.new statements
  end

  private def parse_statement(token : Token) : Statement?
    case token.type
    when .eof?       then nil
    when .let?       then parse_let
    when .return?    then parse_return
    when .semicolon? then parse_statement next_token
    else                  parse_expression_statement token
    end
  end

  private def parse_let : Statement
    name = expect_next(:ident).value.as(String)
    expect_next :equal
    value = parse_expression_statement next_token

    Let.new name, value
  end

  private def parse_return : Statement
    if peek_token.type.semicolon?
      next_token
      Return.new Nop.new
    else
      expr = parse_expression :lowest
      expect_next :semicolon
      Return.new expr
    end
  end

  private def parse_expression_statement(token : Token) : Statement
    expr = parse_expression :lowest
    next_token if peek_token.type.semicolon?

    ExpressionStatement.new expr
  end

  private def parse_expression(prec : Precedence) : Expression
    left = parse_prefix_expression current_token
    raise "cannot parse prefix for type #{current_token.type}" if left.nil?

    while peek_token.type.semicolon? && prec < Precedence.from(peek_token.type)
      _next = parse_prefix_expression next_token
      return left if _next.nil?
      left = parse_infix_expression _next

      next_token
    end

    left
  end

  private def parse_prefix_expression(token : Token) : Expression?
    case token.type
    when .ident?         then parse_identifier token
    when .integer?       then parse_integer token
    when .true?, .false? then parse_boolean token
    when .function?      then parse_function token
    end
  end

  private def parse_infix_expression(left : Expression) : Expression
    op = Infix::Operator.from current_token.type
    prec = Precedence.from current_token.type
    next_token
    right = parse_expression prec

    Infix.new left, op, right
  end

  private def parse_identifier(token : Token) : Expression
    Identifier.new token.value.as(String)
  end

  private def parse_integer(token : Token) : Expression
    IntegerLiteral.new token.value.as(String).to_i64
  end

  private def parse_boolean(token : Token) : Expression
    BooleanLiteral.new token.type.true?
  end

  private def parse_function(token : Token) : Expression
    expect_next :left_paren

    parameters = [] of Identifier
    loop do
      token = next_token
      case token.type
      when .eof?
        raise "unexpected End of File"
      when .ident?
        parameters << parse_identifier token
        expect_next :comma
      when .right_paren?
        break
      else
        raise "unexpected token #{token.type}"
      end
    end

    body = parse_block

    FunctionLiteral.new parameters, body
  end

  private def parse_call(token : Token) : Expression
    name = token.value.as(String)
    expect_next :left_paren
    args = [] of Expression
    return Call.new(name, args) if next_token.type.right_paren?

    args << parse_expression :lowest
    while peek_token.type.comma?
      next_token
      next_token
      args << parse_expression :lowest
    end

    expect_next :right_paren

    Call.new name, args
  end

  private def parse_block : Block
    Block.new [] of Statement
  end

  private def next_token : Token
    @tokens[@pos += 1]
  end

  private def peek_token : Token
    @tokens[@pos + 1]
  end

  private def current_token : Token
    @tokens[@pos]
  end

  private def expect_next(type : Token::Type) : Token
    token = next_token
    raise "expected token #{type}; got #{token.type}" unless token.type == type
    token
  end
end

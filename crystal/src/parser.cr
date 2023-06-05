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
    name = parse_identifier expect_next(:ident)
    expect_next :assign
    value = parse_expression_statement next_token

    Let.new name, value
  end

  private def parse_return : Statement
    return Return.new if next_token.type.semicolon?

    expr = parse_expression :lowest
    expect_next :semicolon unless current_token.type.semicolon?

    Return.new expr
  end

  private def parse_expression_statement(token : Token) : Statement
    expr = parse_expression :lowest
    next_token if peek_token.type.semicolon?

    ExpressionStatement.new expr
  end

  private def parse_expression(prec : Precedence) : Expression
    left = parse_prefix_proc current_token
    raise "cannot parse prefix for type #{current_token.type}" if left.nil?

    while !peek_token.type.semicolon? && prec < Precedence.from(peek_token.type)
      infix = parse_infix_proc peek_token, left
      return left if infix.nil?

      next_token
      left = infix
    end

    left
  end

  private def parse_prefix_proc(token : Token) : Expression?
    case token.type
    when .ident?         then parse_identifier token
    when .integer?       then parse_integer token
    when .bang?, .minus? then parse_prefix_expression
    when .true?, .false? then parse_boolean token
    when .left_paren?    then parse_grouped_expression
    when .function?      then parse_function
    end
  end

  private def parse_infix_proc(token : Token, expr : Expression) : Expression?
    case token.type
    when .plus?, .minus?, .slash?, .asterisk?, .equal?, .not_equal?, .less_than?, .greater_than?
      parse_infix_expression expr
    when .left_paren?
      parse_call expr
    end
  end

  private def parse_prefix_expression : Expression
    op = Prefix::Operator.from current_token.type
    next_token
    right = parse_expression :prefix

    Prefix.new op, right
  end

  private def parse_infix_expression(left : Expression) : Expression
    next_token
    op = Infix::Operator.from current_token.type

    next_token
    prec = Precedence.from current_token.type
    right = parse_expression prec

    Infix.new left, op, right
  end

  private def parse_grouped_expression : Expression
    next_token
    expr = parse_expression :lowest
    expect_next :right_paren unless current_token.type.right_paren?

    expr
  end

  private def parse_identifier(token : Token) : Expression
    Identifier.new token.value
  end

  private def parse_integer(token : Token) : Expression
    IntegerLiteral.new token.value.to_i64
  end

  private def parse_boolean(token : Token) : Expression
    BooleanLiteral.new token.type.true?
  end

  private def parse_function : Expression
    expect_next :left_paren

    parameters = [] of Identifier

    if peek_token.type.right_paren?
      next_token
    else
      token = expect_next :ident
      parameters << parse_identifier token

      while peek_token.type.comma?
        next_token
        next_token
        parameters << parse_identifier current_token
      end

      expect_next :right_paren
    end

    expect_next :left_squirly
    body = parse_block

    FunctionLiteral.new parameters, body
  end

  private def parse_call(expr : Expression) : Expression
    arguments = [] of Expression

    next_token
    if peek_token.type.right_paren?
      next_token
      return Call.new expr, arguments
    end

    next_token
    arguments << parse_expression :lowest
    while peek_token.type.comma?
      next_token
      next_token
      arguments << parse_expression :lowest
    end

    expect_next :right_paren

    Call.new expr, arguments
  end

  private def parse_block : Block
    statements = [] of Statement
    next_token

    until current_token.type.right_squirly? || current_token.type.eof?
      statement = parse_statement current_token
      raise "unexpected End of File" if statement.nil?
      statements << statement
      next_token
    end

    Block.new statements
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

  private def expect_next(*types : Token::Type) : Token
    token = next_token

    unless types.includes? token.type
      message = if types.size > 1
                  "expected one of tokens #{types.join(", ")}"
                else
                  "expected token #{types[0]}"
                end

      raise "#{message}; got #{token.type}"
    end

    token
  end
end

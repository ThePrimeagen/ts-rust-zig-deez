abstract class Node
end

class Statement < Node
end

class Expression < Node
end

class ExpressionStatement < Statement
  property expression : Expression

  def initialize(@expression)
  end
end

class Program < Node
  property statements : Array(Statement)

  def initialize(@statements)
  end
end

class Nop < Expression
end

class Identifier < Expression
  property value : String

  def initialize(@value)
  end
end

class IntegerLiteral < Expression
  property value : Int64

  def initialize(@value)
  end
end

class BooleanLiteral < Expression
  property value : Bool

  def initialize(@value)
  end
end

class FunctionLiteral < Expression
  property parameters : Array(Identifier)
  property body : Block

  def initialize(@parameters, @body)
  end
end

class Call < Expression
  property function : Expression
  property arguments : Array(Expression)

  def initialize(@function, @arguments)
  end
end

class Prefix < Expression
  property right : Expression

  def initialize(@right)
  end
end

class Infix < Expression
  enum Operator
    Equal
    NotEqual
    Plus
    Minus
    Asterisk
    Slash
    LessThan
    GreaterThan
    Unknown

    def self.from(type : Token::Type)
      case type
      when .equal?        then Operator::Equal
      when .not_equal?    then Operator::NotEqual
      when .plus?         then Operator::Plus
      when .minus?        then Operator::Minus
      when .asterisk?     then Operator::Asterisk
      when .slash?        then Operator::Slash
      when .less_than?    then Operator::LessThan
      when .greater_than? then Operator::GreaterThan
      else                     Operator::Unknown
      end
    end
  end

  property left : Expression
  property operator : Operator
  property right : Expression

  def initialize(@left, @operator, @right)
  end
end

class Let < Statement
  property name : Identifier
  property value : ExpressionStatement

  def initialize(@name, @value)
  end
end

class Return < Statement
  property value : Expression

  def initialize(@value)
  end
end

class Block < Statement
  property statements : Array(Statement)

  def initialize(@statements)
  end
end

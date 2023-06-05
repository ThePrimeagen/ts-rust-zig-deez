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

class Identifier < Expression
  property value : String

  def_equals @value

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
  enum Operator
    Negative
    Not
    Unknown

    def self.from(type : Token::Type)
      case type
      when .minus? then Operator::Negative
      when .bang?  then Operator::Not
      else              Operator::Unknown
      end
    end

    def to_s : String
      case self
      when .negative? then "-"
      when .not?      then "!"
      else                 "unknown"
      end
    end
  end

  property operator : Operator
  property right : Expression

  def initialize(@operator, @right)
  end
end

class Infix < Expression
  enum Operator
    Equal
    NotEqual
    Add
    Subtract
    Multiply
    Divide
    LessThan
    GreaterThan
    Unknown

    def self.from(type : Token::Type)
      case type
      when .equal?        then Operator::Equal
      when .not_equal?    then Operator::NotEqual
      when .plus?         then Operator::Add
      when .minus?        then Operator::Subtract
      when .asterisk?     then Operator::Multiply
      when .slash?        then Operator::Divide
      when .less_than?    then Operator::LessThan
      when .greater_than? then Operator::GreaterThan
      else                     Operator::Unknown
      end
    end

    def to_s : String
      case self
      when .equal?        then "=="
      when .not_equal?    then "!="
      when .add?          then "+"
      when .subtract?     then "-"
      when .multiply?     then "*"
      when .divide?       then "/"
      when .less_than?    then "<"
      when .greater_than? then ">"
      else                     "unknown"
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
  property value : Expression?

  def initialize(@value = nil)
  end
end

class Block < Statement
  property statements : Array(Statement)

  def initialize(@statements)
  end
end

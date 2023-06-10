module Evaluator
  extend self

  def evaluate(node : Program, scope : Scope) : BaseValue
    result = NullValue.new

    node.statements.each do |statement|
      result = evaluate statement, scope
    end

    result
  end

  def evaluate(node : Node, scope : Scope) : BaseValue
    evaluate node, scope
  end

  def evaluate(node : Statement, scope : Scope) : BaseValue
    evaluate node, scope
  end

  def evaluate(node : Expression, scope : Scope) : BaseValue
    evaluate node, scope
  end

  def evaluate(node : ExpressionStatement, scope : Scope) : BaseValue
    evaluate node.expression, scope
  end

  def evaluate(node : Identifier, scope : Scope) : BaseValue
    value = scope.get node.value
    return ErrorValue.new "undefined variable '#{node.value}'" if value.nil?

    value
  end

  def evaluate(node : IntegerLiteral, scope : Scope) : BaseValue
    IntegerValue.new node.value
  end

  def evaluate(node : StringLiteral, scope : Scope) : BaseValue
    StringValue.new node.value
  end

  def evaluate(node : BooleanLiteral, scope : Scope) : BaseValue
    BooleanValue.new node.value?
  end

  def evaluate(node : If, scope : Scope) : BaseValue
    condition = evaluate node.condition, scope
    return condition if condition.is_a? ErrorValue

    if condition.is_a?(NullValue) || condition.as?(BooleanValue).try &.value?
      return evaluate node.consequence, scope
    end

    if alternative = node.alternative
      return evaluate alternative, scope
    end

    NullValue.new
  end

  def evaluate(node : FunctionLiteral, scope : Scope) : BaseValue
    FunctionValue.new node.parameters, node.body, scope
  end

  def evaluate(node : Call, scope : Scope) : BaseValue
    function = evaluate node.function, scope
    return function if function.is_a? ErrorValue

    unless function.is_a? FunctionValue
      return ErrorValue.new "cannot call type #{function.type} as a function"
    end

    arguments = node.arguments.map do |argument|
      evaluate argument, scope
    end

    if err = arguments.find &.is_a? ErrorValue
      return err
    end

    unless arguments.size == function.parameters.size
      return ErrorValue.new "expected #{function.parameters.size} arguments to function; got #{arguments.size}"
    end

    child = function.create_scope arguments
    value = evaluate function.body, child

    ReturnValue.new value
  end

  def evaluate(node : Prefix, scope : Scope) : BaseValue
    right = evaluate node.right, scope
    return right if right.is_a? ErrorValue

    case node.operator
    when .negative?
      unless right.is_a? IntegerValue
        return ErrorValue.new "cannot get negative value of type #{right.type}"
      end

      IntegerValue.new -right.value
    when .not?
      unless right.is_a? BooleanValue
        return ErrorValue.new "cannot inverse type #{right.type}"
      end

      BooleanValue.new !right.value?
    else
      ErrorValue.new "unknown prefix operator"
    end
  end

  def evaluate(node : Infix, scope : Scope) : BaseValue
    left = evaluate node.left, scope
    return left if left.is_a? ErrorValue

    right = evaluate node.right, scope
    return right if right.is_a? ErrorValue

    unless left.type == right.type
      return ErrorValue.new "cannot compare types #{left.type} and #{right.type}"
    end

    evaluate_infix left, node.operator, right
  end

  def evaluate(node : Let, scope : Scope) : BaseValue
    value = evaluate node.value, scope
    return value if value.is_a? ErrorValue

    scope.set node.name.value, value

    value
  end

  def evaluate(node : Return, scope : Scope) : BaseValue
    if inner = node.value
      value = evaluate inner, scope
      return value if value.is_a? ErrorValue

      ReturnValue.new value
    else
      ReturnValue.new NullValue.new
    end
  end

  def evaluate(node : Block, scope : Scope) : BaseValue
    result = NullValue.new

    node.statements.each do |statement|
      result = evaluate statement, scope
      break if result.is_a?(ReturnValue | ErrorValue)
    end

    result
  end

  def evaluate_infix(left : IntegerValue, operator : Infix::Operator, right : IntegerValue) : BaseValue
    case operator
    in .equal?
      BooleanValue.new(left.value == right.value)
    in .not_equal?
      BooleanValue.new(left.value != right.value)
    in .add?
      IntegerValue.new(left.value + right.value)
    in .subtract?
      IntegerValue.new(left.value - right.value)
    in .multiply?
      IntegerValue.new(left.value * right.value)
    in .divide?
      IntegerValue.new (left.value / right.value).to_i64
    in .less_than?
      BooleanValue.new(left.value < right.value)
    in .greater_than?
      BooleanValue.new(left.value > right.value)
    in .unknown?
      ErrorValue.new "unknown operator '#{operator}' for type integer"
    end
  end

  def evaluate_infix(left : StringValue, operator : Infix::Operator, right : StringValue) : BaseValue
    if operator.add?
      StringValue.new(left.value + right.value)
    else
      ErrorValue.new "unknown operator '#{operator}' for type string"
    end
  end

  def evaluate_infix(left : BooleanValue, operator : Infix::Operator, right : BooleanValue) : BaseValue
    case operator
    when .equal?
      BooleanValue.new(left.value? == right.value?)
    when .not_equal?
      BooleanValue.new(left.value? != right.value?)
    else
      ErrorValue.new "unknown operator '#{operator}' for type boolean"
    end
  end

  def evaluate_infix(left : ReturnValue, operator : Infix::Operator, right : ReturnValue) : BaseValue
    evaluate_infix left.value, operator, right.value
  end

  def evaluate_infix(left : BaseValue, operator : Infix::Operator, right : BaseValue) : BaseValue
    ErrorValue.new "unknown operator '#{operator}' for types #{left.type} and #{right.type}"
  end
end

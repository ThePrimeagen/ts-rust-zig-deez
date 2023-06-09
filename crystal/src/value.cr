# avoid conflict with top-level Value
abstract class BaseValue
  abstract def type : String
end

class Scope
  @store : Hash(String, BaseValue)
  @outer : Scope?

  def initialize(@outer = nil)
    @store = {} of String => BaseValue

    @store["len"] = BuiltinValue.new(%w[str], self) do |args|
      arg = args[0]
      if arg.is_a? StringValue
        IntegerValue.new arg.value.size
      else
        ErrorValue.new "cannot get length of type #{arg.type}"
      end
    end

    @store["rev"] = BuiltinValue.new(%w[str], self) do |args|
      arg = args[0]
      if arg.is_a? StringValue
        StringValue.new arg.value.reverse
      else
        ErrorValue.new "cannot reverse type #{arg.type}"
      end
    end
  end

  def get(key : String) : BaseValue?
    if value = @store[key]?
      value
    elsif outer = @outer
      outer.get key
    end
  end

  def set(key : String, value : BaseValue) : Nil
    @store[key] = value
  end
end

class IntegerValue < BaseValue
  getter value : Int64

  def initialize(@value)
  end

  def type : String
    "integer"
  end
end

class StringValue < BaseValue
  getter value : String

  def initialize(@value)
  end

  def type : String
    "string"
  end
end

class BooleanValue < BaseValue
  getter? value : Bool

  def initialize(@value)
  end

  def type : String
    "boolean"
  end
end

class FunctionValue < BaseValue
  getter parameters : Array(Identifier)
  getter body : Block
  getter scope : Scope

  def initialize(@parameters, @body, @scope)
  end

  def type : String
    "function"
  end

  def create_scope(arguments : Array(BaseValue)) : Scope
    child = Scope.new @scope

    @parameters.each_with_index do |param, index|
      child.set param.value, arguments[index]
    end

    child
  end
end

class BuiltinValue < BaseValue
  getter parameters : Array(String)
  getter scope : Scope
  @proc : Array(BaseValue) -> BaseValue

  def initialize(@parameters, @scope, &@proc : Array(BaseValue) -> BaseValue)
  end

  def call(arguments : Array(BaseValue)) : BaseValue
    @proc.call arguments
  end

  def create_scope(arguments : Array(BaseValue)) : Scope
    child = Scope.new @scope

    @parameters.each_with_index do |param, index|
      child.set param, arguments[index]
    end

    child
  end

  def type : String
    "builtin function"
  end
end

# TODO: get around cyclical reference so that these can be structs
class ReturnValue < BaseValue
  getter value : BaseValue

  def initialize(@value)
  end

  def type : String
    @value.type
  end
end

class NullValue < BaseValue
  def type : String
    "null"
  end
end

class ErrorValue < BaseValue
  getter message : String

  def initialize(@message)
  end

  def type : String
    "error"
  end
end

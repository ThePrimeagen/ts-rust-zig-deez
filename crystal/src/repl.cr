require "colorize"

Colorize.on_tty_only!

def init_repl : Nil
  {% begin %}
    {% if flag?(:win32) %}
      at_exit do
    {% else %}
      Process.on_interrupt do
    {% end %}
      puts "\nExiting deez NUTS"
      exit 0
    end
  {% end %}

  puts "Welcome to Crystal Deez REPL!".colorize.magenta

  scope = Scope.new

  loop do
    print ">> "
    input = gets || exit 0

    tokens = Lexer.new(input).run
    illegal = tokens.select &.type.illegal?
    unless illegal.empty?
      illegal.each do |token|
        STDERR.puts "#{"Error:".colorize.red} #{token.value}"
      end
      next
    end

    program = Parser.new(tokens).parse
    result = Evaluator.evaluate program, scope
    puts format result
  rescue ex
    STDERR.puts "#{"Error:".colorize.red} #{ex}"
    STDERR.puts
  end
end

private def format(value : IntegerValue)
  value.value.colorize.blue
end

private def format(value : StringValue)
  value.value.inspect.colorize.cyan
end

private def format(value : BooleanValue)
  value.value?.colorize.blue
end

private def format(value : FunctionValue)
  String.build do |io|
    io << "fn".colorize.red
    io << '('

    unless value.parameters.empty?
      io << value.parameters[0].value
      if value.parameters.size > 1
        value.parameters[1..].each do |param|
          io << ", "
          io << param.value
        end
      end
    end

    # TODO: evaluate function body to get accurate return type
    io << ") -> any"
  end
end

private def format(value : ReturnValue)
  format value.value
end

private def format(value : NullValue)
  "null".colorize.light_gray
end

private def format(value : ErrorValue)
  raise value.message
end

private def format(value : BaseValue)
  format value
end

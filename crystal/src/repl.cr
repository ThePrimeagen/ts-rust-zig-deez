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
    print "\n>> "
    input = gets || ""

    offset, quoted = count_delimiters(input, 0, false)
    unless offset == 0
      lines = [input] of String

      loop do
        print ">> "
        input = gets || ""
        lines << input
        offset, quoted = count_delimiters(input, offset, quoted)
        break if offset == 0
      end

      input = lines.join
    end

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
  end
end

private def count_delimiters(input : String, offset : Int32, quoted : Bool) : {Int32, Bool}
  input.chars.each do |char|
    case char
    when '(', '{'
      offset += 1 unless quoted
    when ')', '}'
      offset -= 1 unless quoted
    when '"'
      quoted = !quoted
    end
  end

  {offset, quoted}
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

private def format(value : BuiltinValue)
  String.build do |io|
    io << "builtin fn".colorize.red
    io << '('

    unless value.parameters.empty?
      io << value.parameters[0]
      if value.parameters.size > 1
        value.parameters[1..].each do |param|
          io << ", "
          io << param
        end
      end
    end

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

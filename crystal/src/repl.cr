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
    program.statements.each do |statement|
      puts format statement
      puts
    end
  rescue ex
    STDERR.puts "#{"Error:".colorize.red} #{ex}"
    STDERR.puts
  end
end

private def format(expr : Expression)
  format expr
end

private def format(statement : Statement)
  format statement
end

private def format(statement : ExpressionStatement)
  format statement.expression
end

private def format(expr : Identifier)
  expr.value
end

private def format(expr : IntegerLiteral)
  expr.value.colorize.blue
end

private def format(expr : BooleanLiteral)
  expr.value.colorize.blue
end

private def format(expr : FunctionLiteral)
  String.build do |io|
    io << "fn".colorize.red
    io << '('

    unless expr.parameters.empty?
      io << format_type expr.parameters[0]
      if expr.parameters.size > 1
        expr.parameters[1..].each do |param|
          io << ", "
          io << format_type param
        end
      end
    end

    io << ") -> "
    if last = expr.body.statements.last?
      if last.is_a?(Return) && (value = last.value)
        io << format_type value
      else
        io << "void".colorize.red
      end
    else
      io << "void".colorize.red
    end
  end
end

private def format(expr : Call)
  String.build do |io|
    io << "fn".colorize.red
    io << '('

    unless expr.arguments.empty?
      io << format_type expr.arguments[0]
      if expr.arguments.size > 1
        expr.arguments[1..].each do |arg|
          io << ", "
          io << format_type arg
        end
      end
    end

    io << ") -> unknown"
  end
end

private def format(expr : Prefix)
  return "<invalid prefix>".colorize.red if expr.operator.unknown?

  case expr.operator
  when .negative?
    if expr.right.is_a? IntegerLiteral
      "integer".colorize.blue
    else
      "any"
    end
  when .not?
    "boolean".colorize.blue
  end
end

private def format(expr : Infix)
  return "<invalid prefix>".colorize.red if expr.operator.unknown?

  if expr.left.class == expr.right.class
    format_type expr.right
  else
    "any"
  end
end

private def format(expr : Let)
  String.build do |io|
    io << format expr.name
    io << " : "
    io << format_type expr.value.expression
  end
end

private def format_type(expr : Expression)
  case expr
  when IntegerLiteral
    "integer".colorize.blue
  when BooleanLiteral
    "boolean".colorize.blue
  when FunctionLiteral
    format expr
  else
    "any"
  end
end

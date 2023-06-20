input = """
  let result = add(five, ten);
  =+(){},;
  let five = 5;
  let ten = 10;
  let add = fn(x, y) {
  x + y;
  };
  let result = add(five, ten);
  !-/*5;
  5 < 10 > 5;
  if (5 < 10) {
  return true;
  } else {
  return false;
  }
  10 == 10;
  11 != 10;
  return 5;
  return 10;
  return add(15);
  let returnfoo = 1;
  return returnfoo;
  """

# expected_tokens = Monkey.OldLexer.init(input)
# actual_tokens = Monkey.Lexer.init(input)
# 
# if actual_tokens != expected_tokens do
#   Enum.zip([actual_tokens, expected_tokens])
#   |> Enum.each(fn
#     {t, t} -> IO.inspect(t)
#     {actual, expected} -> 
#       IO.inspect([actual: actual, expected: expected])
#       raise "mismatched output"
#   end)
# end

Benchee.run(
  %{
    # copy + paste old code into `Monkey.OldLexer to compare. Not checked in`
    # "OldLexer" => fn -> Monkey.OldLexer.init(input) end,
    "Lexer" => fn -> Monkey.Lexer.init(input) end
  },
  warmup: 20,
  time: 20,
  memory_time: 5,
  reduction_time: 5
)

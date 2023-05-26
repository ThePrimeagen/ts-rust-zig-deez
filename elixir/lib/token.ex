defmodule Monkey.Token do
  @enforce_keys [:type, :literal]
  defstruct [:type, :literal]

  @type t :: %{
          type: atom(),
          literal: binary()
        }

  @keywords %{
    "fn" => :fn,
    "let" => :let,
    "true" => true,
    "false" => false,
    "if" => :if,
    "else" => :else,
    "return" => :return
  }

  @types %{
    illegal: "ILLEGAL",
    eof: "EOF",
    # identifiers and literals
    ident: "IDENT",
    # 123
    int: "INT",
    # operators
    assign: "=",
    plus: "+",
    minus: "-",
    asterisk: "*",
    slash: "/",
    bang: "!",
    equal_equal: "==",
    not_equal: "!=",
    greater_than: ">",
    less_than: "<",
    # delimiters
    comma: ",",
    semicolon: ",",
    lparen: "(",
    rparen: ")",
    lbrace: "{",
    rbrace: "}",
    fn: "FUNCTION",
    let: "LET",
    true: "TRUE",
    false: "FALSE",
    if: "IF",
    else: "ELSE",
    return: "RETURN"
  }

  def new(type, literal) when is_atom(type) and is_binary(literal) do
    if Map.has_key?(@types, type) do
      %__MODULE__{type: type, literal: literal}
    else
      raise "Unsupported token type: #{inspect(type)}"
    end
  end

  def lookup_identifier(ident) do
    Map.get(@keywords, ident, :ident)
  end
end

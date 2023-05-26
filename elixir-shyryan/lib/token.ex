defmodule Monkey.Token do
  @moduledoc """
  Token struct for the Monkey lang.
  """

  @type token_type ::
          :illegal
          | :eof
          | :ident
          | :int
          | :assign
          | :plus
          | :minus
          | :asterisk
          | :slash
          | :bang
          | :equal_equal
          | :not_equal
          | :greater_than
          | :less_than
          | :comma
          | :semicolon
          | :lparen
          | :rparen
          | :lsquirly
          | :rsquirly
          | :fn
          | :let
          | true
          | false
          | :if
          | :else
          | :return

  @type t :: %__MODULE__{
          type: token_type(),
          literal: String.t()
        }

  @enforce_keys [:type, :literal]
  defstruct [:type, :literal]

  @keywords %{
    "fn" => :fn,
    "let" => :let,
    "true" => true,
    "false" => false,
    "if" => :if,
    "else" => :else,
    "return" => :return
  }

  @literals %{
    illegal: "ILLEGAL",
    eof: "",
    ident: "IDENT",
    int: "INT",
    # Operators
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
    # Delimiters
    comma: ",",
    semicolon: ";",
    lparen: "(",
    rparen: ")",
    lsquirly: "{",
    rsquirly: "}",
    fn: "FUNCTION",
    let: "LET",
    true: "TRUE",
    false: "FALSE",
    if: "IF",
    else: "ELSE",
    return: "RETURN"
  }

  @token_types Map.keys(@literals)

  @doc """
  Create a new token struct.
  """
  @spec new(token_type()) :: t()
  def new(type) when type in @token_types do
    new(type, Map.fetch!(@literals, type))
  end

  @spec new(token_type(), String.t()) :: t()
  def new(type, literal) when type in @token_types and is_binary(literal) do
    %__MODULE__{type: type, literal: literal}
  end

  @doc """
  Lookup an identifier.
  """
  @spec lookup_identifier(String.t()) :: atom()
  def lookup_identifier(identifier) do
    Map.get(@keywords, identifier, :ident)
  end
end

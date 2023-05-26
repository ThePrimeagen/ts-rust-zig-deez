defmodule Monkey.Lexer do
  @moduledoc false
  alias Monkey.Token

  def lex(input) do
    input
    |> String.split("", trim: true)
    |> tokenize([])
  end

  def tokenize(_chars = [], tokens) do
    Enum.reverse([Token.new(:eof, "") | tokens])
  end

  def tokenize(chars = [ch | rest], tokens) do
    cond do
      is_whitespace(ch) -> tokenize(rest, tokens)
      is_letter(ch) -> read_identifier(chars, tokens)
      is_digit(ch) -> read_number(chars, tokens)
      is_op(chars) -> read_op(chars, tokens)
      is_return(chars) -> read_return(chars, tokens)
      true -> read_next(chars, tokens)
    end
  end

  def read_identifier(chars, tokens) do
    {identifier, rest} = Enum.split_while(chars, &is_letter/1)
    identifier = Enum.join(identifier)
    token = Token.new(Token.lookup_identifier(identifier), identifier)
    tokenize(rest, [token | tokens])
  end

  def read_number(chars, tokens) do
    {number, rest} = Enum.split_while(chars, &is_digit/1)
    number = Enum.join(number)
    token = Token.new(:int, number)
    tokenize(rest, [token | tokens])
  end

  def read_op(chars, tokens) do
    {op, rest} = Enum.split(chars, 2)
    op = Enum.join(op)

    token =
      case op do
        "==" -> Token.new(:equal_equal, op)
        "!=" -> Token.new(:not_equal, op)
      end

    tokenize(rest, [token | tokens])
  end

  def read_return(chars, tokens) do
    {ret, rest} = Enum.split(chars, 5)
    ret = Enum.join(ret)
    token = Token.new(:return, ret)
    tokenize(rest, [token | tokens])
  end

  def read_next(_chars = [ch | rest], tokens) do
    token =
      case ch do
        ";" -> Token.new(:semicolon, ch)
        "," -> Token.new(:comma, ch)
        "(" -> Token.new(:lparen, ch)
        ")" -> Token.new(:rparen, ch)
        "{" -> Token.new(:lbrace, ch)
        "}" -> Token.new(:rbrace, ch)
        "=" -> Token.new(:assign, ch)
        "+" -> Token.new(:plus, ch)
        "-" -> Token.new(:minus, ch)
        "!" -> Token.new(:bang, ch)
        "/" -> Token.new(:slash, ch)
        "*" -> Token.new(:asterisk, ch)
        ">" -> Token.new(:greater_than, ch)
        "<" -> Token.new(:less_than, ch)
        _ -> Token.new(:illegal, "")
      end

    tokenize(rest, [token | tokens])
  end

  def is_whitespace(ch) do
    ch == " " || ch == "\n" || ch == "\t"
  end

  def is_letter(ch) do
    ("a" <= ch && ch <= "z") || ("A" <= ch && ch <= "Z") || ch == "_"
  end

  def is_digit(ch) do
    "0" <= ch && ch <= "9"
  end

  def is_op(chars) do
    (Enum.at(chars, 0) == "!" || Enum.at(chars, 0) == "=") && Enum.at(chars, 1) == "="
  end

  def is_return(chars) do
    {ret, _rest} = Enum.split(chars, 5)
    ret = Enum.join(ret)
    ret == "return"
  end
end

defmodule Monkey.Lexer do
  @moduledoc """
  The Monkey Lang lexer in Elixir.
  """

  alias Monkey.Token

  defguardp is_whitespace(c) when c in ~c[ \n\t]
  defguardp is_letter(c) when c in ?a..?z or c in ?A..?Z or c == ?_
  defguardp is_digit(c) when c in ?0..?9

  @doc """
  Turn some input into a list of tokens.

  ## Example

      iex> Lexer.init("let five = 5;")
      [
        %Token{type: :let, literal: "let"},
        %Token{type: :ident, literal: "five"},
        %Token{type: :assign, literal: "="},
        %Token{type: :int, literal: "5"},
        %Token{type: :semicolon, literal: ";"},
        %Token{type: :eof, literal: ""}
      ]

  """
  @spec init(String.t()) :: [Token.t()]
  def init(input) when is_binary(input) do
    tokenize(input, [])
  end

  # Recursive base-case. When the input is empty, then we add an EOF token.
  defp tokenize(<<>>, tokens) do
    [Token.new(:eof) | tokens] |> Enum.reverse()
  end

  # Ignore whitespace.
  defp tokenize(<<c::8, rest::binary>>, tokens) when is_whitespace(c),
    do: tokenize(rest, tokens)

  # Recursively go through the input, tokenizing each character.
  # Uses binary pattern-matching to match on the first character(s).
  # See: https://hexdocs.pm/elixir/Kernel.SpecialForms.html#<<>>/1
  defp tokenize(input, tokens) do
    {token, rest} =
      case input do
        <<c::8, _::binary>> when is_letter(c) -> read_identifier(input)
        <<c::8, _::binary>> when is_digit(c) -> read_number(input)
        <<"==", rest::binary>> -> {Token.new(:equal_equal), rest}
        <<"!=", rest::binary>> -> {Token.new(:not_equal), rest}
        <<";", rest::binary>> -> {Token.new(:semicolon), rest}
        <<",", rest::binary>> -> {Token.new(:comma), rest}
        <<"(", rest::binary>> -> {Token.new(:lparen), rest}
        <<")", rest::binary>> -> {Token.new(:rparen), rest}
        <<"{", rest::binary>> -> {Token.new(:lsquirly), rest}
        <<"}", rest::binary>> -> {Token.new(:rsquirly), rest}
        <<"=", rest::binary>> -> {Token.new(:assign), rest}
        <<"+", rest::binary>> -> {Token.new(:plus), rest}
        <<"-", rest::binary>> -> {Token.new(:minus), rest}
        <<"!", rest::binary>> -> {Token.new(:bang), rest}
        <<"/", rest::binary>> -> {Token.new(:slash), rest}
        <<"*", rest::binary>> -> {Token.new(:asterisk), rest}
        <<">", rest::binary>> -> {Token.new(:greater_than), rest}
        <<"<", rest::binary>> -> {Token.new(:less_than), rest}
        <<"return", rest::binary>> -> {Token.new(:return), rest}
        <<_::8, rest::binary>> -> {Token.new(:illegal), rest}
      end

    tokenize(rest, [token | tokens])
  end

  defp read_identifier(input) do
    {identifier, rest} = read_identifier(input, "")

    token =
      identifier
      |> Token.lookup_identifier()
      |> Token.new(identifier)

    {token, rest}
  end

  defp read_identifier(<<c::8, rest::binary>>, acc) when is_letter(c) do
    read_identifier(rest, acc <> <<c>>)
  end

  defp read_identifier(rest, identifier), do: {identifier, rest}

  defp read_number(input) do
    {number, rest} = read_number(input, "")
    {Token.new(:int, number), rest}
  end

  defp read_number(<<c::8, rest::binary>>, acc) when is_digit(c) do
    read_number(rest, acc <> <<c>>)
  end

  defp read_number(rest, number), do: {number, rest}
end

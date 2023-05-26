defmodule Monkey.Lexer do
  @moduledoc false

  alias Monkey.Token

  defguardp is_whitespace(c) when c in ~c[ \n\t]
  defguardp is_letter(c) when c in ?a..?z or c in ?A..?Z or c == ?_
  defguardp is_digit(c) when c in ?0..?9

  @doc """
  Turn some input into a list of tokens.
  """
  @spec lex(String.t()) :: [Token.t()]
  def lex(input) when is_binary(input) do
    tokenize(input, [])
  end

  # Recursive base-case. When the input is empty, then we add an EOF token.
  defp tokenize(<<>>, tokens) do
    [Token.new(:eof) | tokens] |> Enum.reverse()
  end

  # Recursively go through the input, tokenizing each character.
  # Uses binary pattern-matching to match on the first character(s).
  defp tokenize(input, tokens) do
    case input do
      <<c::8, _::binary>> when is_letter(c) -> read_identifier(input, tokens)
      <<c::8, _::binary>> when is_digit(c) -> read_number(input, tokens)
      <<"==", rest::binary>> -> tokenize(rest, [Token.new(:equal_equal) | tokens])
      <<"!=", rest::binary>> -> tokenize(rest, [Token.new(:not_equal) | tokens])
      <<";", rest::binary>> -> tokenize(rest, [Token.new(:semicolon) | tokens])
      <<",", rest::binary>> -> tokenize(rest, [Token.new(:comma) | tokens])
      <<"(", rest::binary>> -> tokenize(rest, [Token.new(:lparen) | tokens])
      <<")", rest::binary>> -> tokenize(rest, [Token.new(:rparen) | tokens])
      <<"{", rest::binary>> -> tokenize(rest, [Token.new(:lsquirly) | tokens])
      <<"}", rest::binary>> -> tokenize(rest, [Token.new(:rsquirly) | tokens])
      <<"=", rest::binary>> -> tokenize(rest, [Token.new(:assign) | tokens])
      <<"+", rest::binary>> -> tokenize(rest, [Token.new(:plus) | tokens])
      <<"-", rest::binary>> -> tokenize(rest, [Token.new(:minus) | tokens])
      <<"!", rest::binary>> -> tokenize(rest, [Token.new(:bang) | tokens])
      <<"/", rest::binary>> -> tokenize(rest, [Token.new(:slash) | tokens])
      <<"*", rest::binary>> -> tokenize(rest, [Token.new(:asterisk) | tokens])
      <<">", rest::binary>> -> tokenize(rest, [Token.new(:greater_than) | tokens])
      <<"<", rest::binary>> -> tokenize(rest, [Token.new(:less_than) | tokens])
      <<"return", rest::binary>> -> tokenize(rest, [Token.new(:return) | tokens])
      <<_::8, rest::binary>> -> tokenize(rest, [Token.new(:illegal) | tokens])
    end
  end

  defp read_identifier(input, tokens) do
    {identifier, rest} = accumulate_identifier(input, "")

    token =
      identifier
      |> Token.lookup_identifier()
      |> Token.new()

    tokenize(rest, [token | tokens])
  end

  defp accumulate_identifier(<<c::8, rest::binary>>, acc) when is_letter(c) do
    accumulate_identifier(rest, acc <> <<c>>)
  end

  defp accumulate_identifier(rest, identifier), do: {identifier, rest}

  defp read_number(input, tokens) do
    {number, rest} = accumulate_number(input, "")
    tokenize(rest, [Token.new(:int) | tokens])
  end

  defp accumulate_number(rest, number) , do: {number, rest}

  defp accumulate_number(<<c::8, rest::binary>>, acc) when is_digit(c) do
    accumulate_number(rest, acc <> <<c>>)
  end
end

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
  def lex(input) do
    input
    |> tokenize([])
    |> Enum.reverse()
  end

  defp tokenize(<<>>, tokens) do
    [Token.new(:eof) | tokens]
  end

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
      <<"{", rest::binary>> -> tokenize(rest, [Token.new(:lbrace) | tokens])
      <<"}", rest::binary>> -> tokenize(rest, [Token.new(:rbrace) | tokens])
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

  defp accumulate_identifier(rest, identifier) when not is_letter(c), do: {identifier, rest}
  defp accumulate_identifier(<<c::binary-size(1), rest::binary>>, identifier, tokens) do
    accumulate_identifier(rest, identifier <> c, tokens)
  end

  defp read_number(input, tokens) do
    {number, rest} = accumulate_number(input, "")
    tokenize(rest, [Token.new(:int, number) | tokens])
  end

  defp accumulate_number(rest, number) when not is_digit(c), do: {number, rest}
  defp accumulate_number(<<c::binary-size(1), rest::binary>>, tokens) do
    accumulate_number(rest, number <> c, tokens)
  end
end

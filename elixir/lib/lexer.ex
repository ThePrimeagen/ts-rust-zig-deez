defmodule Monkey.Lexer do
  @moduledoc """
  The Monkey lexer in Elixir.

  The lame comments are only to help explain for people not familiar with
  Elixir. They wouldn't normally be needed.
  """

  # Specifying types is optional but not only does it help document the code,
  # it helps make the LSP better and improves the accuracy of static analysis
  # tools like dialyzer.
  @type keyword_token ::
          :function
          | :let
          | :if
          | :else
          | :return
          | true
          | false

  @type token ::
          :assign
          | :plus
          | :minus
          | :asterisk
          | :slash
          | :bang
          | :equal
          | :not_equal
          | :greater_than
          | :less_than
          | :comma
          | :semicolon
          | :lparen
          | :rparen
          | :lsquirly
          | :rsquirly
          | :eof
          | keyword_token()
          | {:ident, String.t()}
          | {:int, String.t()}
          | {:illegal, String.t()}

  # Generate private macros suitable for use in guard expressions.
  # See: https://hexdocs.pm/elixir/Kernel.html#defguard/1
  defguardp is_whitespace(c) when c in ~c[ \n\t]
  defguardp is_letter(c) when c in ?a..?z or c in ?A..?Z or c == ?_
  defguardp is_digit(c) when c in ?0..?9

  @doc """
  Turn some input into a list of tokens.

  ## Example

      iex> Lexer.init("let five = 5;")
      [
        :let,
        {:ident, "five"},
        :assign,
        {:int, "5"},
        :semicolon,
        :eof
      ]

  """
  @spec init(String.t()) :: [token()]
  def init(input) when is_binary(input) do
    lex(input, [])
  end

  # Recursive base-case. When the input is empty, we add an EOF token.
  #
  # It is more efficient to prepend to a list and reverse it than to append to
  # the list while building it. Lists here are linked-lists, so the whole list
  # would be copied each time we append to it.
  # See: https://www.erlang.org/doc/efficiency_guide/listhandling
  @spec lex(input :: String.t(), [token()]) :: [token()]
  defp lex(<<>>, tokens) do
    [:eof | tokens] |> Enum.reverse()
  end

  # Ignore whitespace.
  defp lex(<<c::8, rest::binary>>, tokens) when is_whitespace(c) do
    lex(rest, tokens)
  end

  # Tail-recursively go through the input, tokenizing the character(s).
  defp lex(input, tokens) do
    {token, rest} = tokenize(input)
    lex(rest, [token | tokens])
  end

  # Uses binary pattern-matching to match on the first character(s).
  # e.g. `<<c::8, rest::binary>>` matches on the first 8 bits and assigns it
  # to `c`, then assigns the rest of the binary to `rest`.
  # For more details, see: https://hexdocs.pm/elixir/Kernel.SpecialForms.html#<<>>/1
  @spec tokenize(input :: String.t()) :: {token(), rest :: String.t()}
  defp tokenize(<<"==", rest::binary>>), do: {:equal, rest}
  defp tokenize(<<"!=", rest::binary>>), do: {:not_equal, rest}
  defp tokenize(<<";", rest::binary>>), do: {:semicolon, rest}
  defp tokenize(<<",", rest::binary>>), do: {:comma, rest}
  defp tokenize(<<"(", rest::binary>>), do: {:lparen, rest}
  defp tokenize(<<")", rest::binary>>), do: {:rparen, rest}
  defp tokenize(<<"{", rest::binary>>), do: {:lsquirly, rest}
  defp tokenize(<<"}", rest::binary>>), do: {:rsquirly, rest}
  defp tokenize(<<"=", rest::binary>>), do: {:assign, rest}
  defp tokenize(<<"+", rest::binary>>), do: {:plus, rest}
  defp tokenize(<<"-", rest::binary>>), do: {:minus, rest}
  defp tokenize(<<"!", rest::binary>>), do: {:bang, rest}
  defp tokenize(<<"/", rest::binary>>), do: {:slash, rest}
  defp tokenize(<<"*", rest::binary>>), do: {:asterisk, rest}
  defp tokenize(<<">", rest::binary>>), do: {:greater_than, rest}
  defp tokenize(<<"<", rest::binary>>), do: {:less_than, rest}
  defp tokenize(<<c::8, rest::binary>>) when is_letter(c), do: read_identifier(rest, <<c>>)
  defp tokenize(<<c::8, rest::binary>>) when is_digit(c), do: read_number(rest, <<c>>)
  defp tokenize(<<c::8, rest::binary>>), do: {{:illegal, <<c>>}, rest}

  # Recursively read the input until we hit a non-letter character. Builds an
  # iolist, then tokenizes the word.
  @spec read_identifier(String.t(), iodata()) :: {token(), String.t()}
  defp read_identifier(<<c::8, rest::binary>>, acc) when is_letter(c) do
    read_identifier(rest, [acc | <<c>>])
  end

  defp read_identifier(rest, acc) do
    {IO.iodata_to_binary(acc) |> tokenize_word(), rest}
  end

  # Recursively read the input until we hit a non-digit character. Builds an
  # iolist, then tokenizes the number.
  @spec read_number(String.t(), iodata()) :: {token(), String.t()}
  defp read_number(<<c::8, rest::binary>>, acc) when is_digit(c) do
    read_number(rest, [acc | <<c>>])
  end

  defp read_number(rest, acc) do
    {{:int, IO.iodata_to_binary(acc)}, rest}
  end

  # Tokenize the word. Checks if it is a keyword, otherwise it is an
  # identifier.
  @spec tokenize_word(String.t()) :: keyword_token() | {:ident, String.t()}
  defp tokenize_word("fn"), do: :function
  defp tokenize_word("let"), do: :let
  defp tokenize_word("if"), do: :if
  defp tokenize_word("else"), do: :else
  defp tokenize_word("true"), do: true
  defp tokenize_word("false"), do: false
  defp tokenize_word("return"), do: :return
  defp tokenize_word(ident), do: {:ident, ident}
end

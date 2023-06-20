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

      iex> Monkey.Lexer.init("let five = 5;")
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

  @spec lex(input :: String.t(), [token()]) :: [token()]
  def lex(input, tokens) do
    # Tail-recursively go through the input, tokenizing the character(s).
    #
    # We are using pure tail recursion, rather than a tail recursive main function with a helper to extract tokens.
    # Everything gets given `tokens`, and is responsible for calling `lex/2` to continue iterating over `input`, and
    # does not return until :eof is hit.
    # This improves performance in these ways:
    # - avoid work to assemble a tuple of {token, rest} to allow multiple return values
    # - avoid work to pattern match to extract values from the {token, rest} return tuple
    # - not allocating that tuple on the heap - so 0 heap memory is allocated at all (or needs to GCed later)
    # - BEAM uses `call_only` instructions only, so no stack allocation needed, or continuation pointer management etc
    #   needed, and all arguments are passed via BEAM registers
    #
    # By doing all matching in one place here, the compiler will generate optimised lookup on the matched input. If
    # this is spread out between a main function, and a helper to tokenize, it needs to do the match multiple times, and
    # it won't be as well optimised due to having less context on what the execution paths would be
    #
    # It is more efficient to prepend to a list and reverse it than to append to
    # the list while building it. Lists here are linked-lists, so the whole list
    # would be copied each time we append to it.
    # See: https://www.erlang.org/doc/efficiency_guide/listhandling
    #
    # Uses binary pattern-matching to match on the first character(s).
    # e.g. `<<c::8, rest::binary>>` matches on the first 8 bits and assigns it
    # to `c`, then assigns the rest of the binary to `rest`.
    # For more details, see: https://hexdocs.pm/elixir/Kernel.SpecialForms.html#<<>>/1
    #
    # this logic works equivalent if done as multiple function heads, or a case statement (the compiled BEAM code is
    # identical either way). The benefit of a case statement is each case clause only has to worry about the string
    # match, and doesn't have to cart around `tokens`, as we've already done that when we entered the function.
    case input do
      # Recursive base-case. When the input is empty, we add an EOF token.
      <<>> -> [:eof | tokens] |> Enum.reverse()
      # Ignore whitespace.
      <<c::8, rest::binary>> when is_whitespace(c) -> lex(rest, tokens)
      <<"==", rest::binary>> -> lex(rest, [:equal | tokens])
      <<"!=", rest::binary>> -> lex(rest, [:not_equal | tokens])
      <<";", rest::binary>> -> lex(rest, [:semicolon | tokens])
      <<",", rest::binary>> -> lex(rest, [:comma | tokens])
      <<"(", rest::binary>> -> lex(rest, [:lparen | tokens])
      <<")", rest::binary>> -> lex(rest, [:rparen | tokens])
      <<"{", rest::binary>> -> lex(rest, [:lsquirly | tokens])
      <<"}", rest::binary>> -> lex(rest, [:rsquirly | tokens])
      <<"=", rest::binary>> -> lex(rest, [:assign | tokens])
      <<"+", rest::binary>> -> lex(rest, [:plus | tokens])
      <<"-", rest::binary>> -> lex(rest, [:minus | tokens])
      <<"!", rest::binary>> -> lex(rest, [:bang | tokens])
      <<"/", rest::binary>> -> lex(rest, [:slash | tokens])
      <<"*", rest::binary>> -> lex(rest, [:asterisk | tokens])
      <<">", rest::binary>> -> lex(rest, [:greater_than | tokens])
      <<"<", rest::binary>> -> lex(rest, [:less_than | tokens])
      <<"fn", rest::binary>> -> maybe_keyword(rest, byte_size("fn"), input, :function, tokens)
      <<"let", rest::binary>> -> maybe_keyword(rest, byte_size("let"), input, :let, tokens)
      <<"if", rest::binary>> -> maybe_keyword(rest, byte_size("if"), input, :if, tokens)
      <<"else", rest::binary>> -> maybe_keyword(rest, byte_size("else"), input, :else, tokens)
      <<"true", rest::binary>> -> maybe_keyword(rest, byte_size("true"), input, true, tokens)
      <<"false", rest::binary>> -> maybe_keyword(rest, byte_size("false"), input, false, tokens)
      <<"return", rest::binary>> -> maybe_keyword(rest, byte_size("return"), input, :return, tokens)
      <<c::8, rest::binary>> when is_letter(c) -> identifier(rest, 1, input, tokens)
      <<c::8, rest::binary>> when is_digit(c) -> number(rest, 1, input, tokens)
      <<c::8, rest::binary>> -> lex(rest, [{:illegal, <<c>>} | tokens])
    end
  end

  # we have already matched that we have the start of a keyword. Check the next character to see if it indicates the
  # keyword token end. If so, then we can just return the token directly. If not, then it's a plain identifier, and
  @spec maybe_keyword(String.t(), integer(), String.t(), token(), [token()]) :: [token()]
  defp maybe_keyword(<<c::8, rest::binary>>, ident_len, input, _keyword, tokens)
       when is_letter(c) do
    # we have another letter, so this isn't a keyword - but an identifier that starts with the same characters. Tokenize it as such.
    identifier(rest, ident_len + 1, input, tokens)
  end

  defp maybe_keyword(rest, _ident_len, _input, keyword, tokens) do
    # next char wasn't a letter, so this is actually a keyword
    lex(rest, [keyword | tokens])
  end

  # Recursively read the input until we hit a non-letter character.
  #
  # Rather than extracting a single character at a time via pattern matching and storing them in an accumulator,
  # instead track how many characters we are into the original input as we go. Then, when the token end is detected,
  # do a single binary pattern match of the specified length to pull out a sub binary. This means we don't accumulate
  # garbage or allocate anything on the heap as we go. The sub binary will be a simple reference into the original
  # input, which is very fast, and memory efficient.
  @spec identifier(String.t(), integer(), String.t(), [token()]) :: [token()]
  defp identifier(<<c::8, rest::binary>>, ident_len, input, tokens) when is_letter(c) do
    identifier(rest, ident_len + 1, input, tokens)
  end

  defp identifier(_rest, ident_len, input, tokens) do
    <<ident::bytes-size(ident_len), rest::binary>> = input
    lex(rest, [{:ident, ident} | tokens])
  end

  # Recursively read the input until we hit a non-digit character.
  @spec number(String.t(), integer(), String.t(), [token()]) :: [token()]
  defp number(<<c::8, rest::binary>>, number_len, input, tokens) when is_digit(c) do
    number(rest, number_len + 1, input, tokens)
  end

  defp number(_rest, number_len, input, tokens) do
    <<number::bytes-size(number_len), rest::binary>> = input
    lex(rest, [{:int, number} | tokens])
  end
end

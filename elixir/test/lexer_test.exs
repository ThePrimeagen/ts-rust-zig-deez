defmodule LexerTest do
  use ExUnit.Case

  alias Monkey.Lexer
  alias Monkey.Token

  describe "lex/1" do
    test "tokenizes simple set of syntax symbols" do
      result = "=+(){},;" |> Lexer.lex()

      assert result == [
               %Token{type: :assign, literal: "="},
               %Token{type: :plus, literal: "+"},
               %Token{type: :lparen, literal: "("},
               %Token{type: :rparen, literal: ")"},
               %Token{type: :lbrace, literal: "{"},
               %Token{type: :rbrace, literal: "}"},
               %Token{type: :comma, literal: ","},
               %Token{type: :semicolon, literal: ";"},
               %Token{type: :eof, literal: ""}
             ]
    end

    test "tokenizes variable declarations" do
      input = """
      let five = 5;
      let ten = 10;
      """

      result = input |> Lexer.lex()

      assert result == [
               %Token{type: :let, literal: "let"},
               %Token{type: :ident, literal: "five"},
               %Token{type: :assign, literal: "="},
               %Token{type: :int, literal: "5"},
               %Token{type: :semicolon, literal: ";"},
               %Token{type: :let, literal: "let"},
               %Token{type: :ident, literal: "ten"},
               %Token{type: :assign, literal: "="},
               %Token{type: :int, literal: "10"},
               %Token{type: :semicolon, literal: ";"},
               %Token{type: :eof, literal: ""}
             ]
    end

    test "tokenizes function expression" do
      input = """
      let add = fn(x, y) {
        x + y;
      };
      """

      result = input |> Lexer.lex()

      assert result == [
               %Token{type: :let, literal: "let"},
               %Token{type: :ident, literal: "add"},
               %Token{type: :assign, literal: "="},
               %Token{type: :fn, literal: "fn"},
               %Token{type: :lparen, literal: "("},
               %Token{type: :ident, literal: "x"},
               %Token{type: :comma, literal: ","},
               %Token{type: :ident, literal: "y"},
               %Token{type: :rparen, literal: ")"},
               %Token{type: :lbrace, literal: "{"},
               %Token{type: :ident, literal: "x"},
               %Token{type: :plus, literal: "+"},
               %Token{type: :ident, literal: "y"},
               %Token{type: :semicolon, literal: ";"},
               %Token{type: :rbrace, literal: "}"},
               %Token{type: :semicolon, literal: ";"},
               %Token{type: :eof, literal: ""}
             ]
    end

    test "tokenizes function call, result assigned to variable" do
      input = """
      let result = add(five, ten);
      """

      result = input |> Lexer.lex()

      assert result == [
               %Token{type: :let, literal: "let"},
               %Token{type: :ident, literal: "result"},
               %Token{type: :assign, literal: "="},
               %Token{type: :ident, literal: "add"},
               %Token{type: :lparen, literal: "("},
               %Token{type: :ident, literal: "five"},
               %Token{type: :comma, literal: ","},
               %Token{type: :ident, literal: "ten"},
               %Token{type: :rparen, literal: ")"},
               %Token{type: :semicolon, literal: ";"},
               %Token{type: :eof, literal: ""}
             ]
    end

    test "tokenizes operators" do
      input = """
      !-/*5;
      5 < 10 > 5;
      """

      result = input |> Lexer.lex()

      assert result == [
               %Token{type: :bang, literal: "!"},
               %Token{type: :minus, literal: "-"},
               %Token{type: :slash, literal: "/"},
               %Token{type: :asterisk, literal: "*"},
               %Token{type: :int, literal: "5"},
               %Token{type: :semicolon, literal: ";"},
               %Token{type: :int, literal: "5"},
               %Token{type: :less_than, literal: "<"},
               %Token{type: :int, literal: "10"},
               %Token{type: :greater_than, literal: ">"},
               %Token{type: :int, literal: "5"},
               %Token{type: :semicolon, literal: ";"},
               %Token{type: :eof, literal: ""}
             ]
    end

    test "tokenizes if/else statement" do
      input = """
      if (5 < 10) {
        return true;
      } else {
        return false;
      }
      """

      result = input |> Lexer.lex()

      assert result == [
               %Token{type: :if, literal: "if"},
               %Token{type: :lparen, literal: "("},
               %Token{type: :int, literal: "5"},
               %Token{type: :less_than, literal: "<"},
               %Token{type: :int, literal: "10"},
               %Token{type: :rparen, literal: ")"},
               %Token{type: :lbrace, literal: "{"},
               %Token{type: :return, literal: "return"},
               %Token{type: true, literal: "true"},
               %Token{type: :semicolon, literal: ";"},
               %Token{type: :rbrace, literal: "}"},
               %Token{type: :else, literal: "else"},
               %Token{type: :lbrace, literal: "{"},
               %Token{type: :return, literal: "return"},
               %Token{type: false, literal: "false"},
               %Token{type: :semicolon, literal: ";"},
               %Token{type: :rbrace, literal: "}"},
               %Token{type: :eof, literal: ""}
             ]
    end

    test "tokenizes equivalence operators" do
      input = """
      10 == 10;
      11 != 10;
      """

      result = input |> Lexer.lex()

      assert result == [
               %Token{type: :int, literal: "10"},
               %Token{type: :equal_equal, literal: "=="},
               %Token{type: :int, literal: "10"},
               %Token{type: :semicolon, literal: ";"},
               %Token{type: :int, literal: "11"},
               %Token{type: :not_equal, literal: "!="},
               %Token{type: :int, literal: "10"},
               %Token{type: :semicolon, literal: ";"},
               %Token{type: :eof, literal: ""}
             ]
    end

    test "tokenizes return statements" do
      input = """
      return 5;
      return 10;
      return add(15);
      """

      result = input |> Lexer.lex()

      assert result == [
               %Token{type: :return, literal: "return"},
               %Token{type: :int, literal: "5"},
               %Token{type: :semicolon, literal: ";"},
               %Token{type: :return, literal: "return"},
               %Token{type: :int, literal: "10"},
               %Token{type: :semicolon, literal: ";"},
               %Token{type: :return, literal: "return"},
               %Token{type: :ident, literal: "add"},
               %Token{type: :lparen, literal: "("},
               %Token{type: :int, literal: "15"},
               %Token{type: :rparen, literal: ")"},
               %Token{type: :semicolon, literal: ";"},
               %Token{type: :eof, literal: ""}
             ]
    end
  end
end

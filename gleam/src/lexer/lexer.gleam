import gleam/iterator
import gleam/string
import token/token

pub fn init(input) {
  lex(input)
}

fn lex(input) {
  input
  |> iterator.unfold(with: fn(source) {
    case next_token(source) {
      #("", token.Eof) -> iterator.Done
      #(rest, token) -> iterator.Next(element: token, accumulator: rest)
    }
  })
  |> iterator.to_list()
}

fn next_token(source) {
  case source {
    // End of file 
    "" -> #(source, token.Eof)

    // Whitespace characters (to be ignored)
    " " <> rest | "\t" <> rest | "\n" <> rest | "\r" <> rest -> next_token(rest)

    // Delimiters
    "," <> rest -> #(rest, token.Comma)
    ";" <> rest -> #(rest, token.Semicolon)
    ":" <> rest -> #(rest, token.Colon)
    "(" <> rest -> #(rest, token.LeftParen)
    ")" <> rest -> #(rest, token.RightParen)
    "{" <> rest -> #(rest, token.LeftBrace)
    "}" <> rest -> #(rest, token.RightBrace)
    "[" <> rest -> #(rest, token.LeftBracket)
    "]" <> rest -> #(rest, token.RightBracket)

    // Operators
    "==" <> rest -> #(rest, token.Equal)
    "=" <> rest -> #(rest, token.Assign)
    "!=" <> rest -> #(rest, token.NotEqual)
    "!" <> rest -> #(rest, token.Bang)
    "+" <> rest -> #(rest, token.Plus)
    "-" <> rest -> #(rest, token.Minus)
    "*" <> rest -> #(rest, token.Asterisk)
    "/" <> rest -> #(rest, token.Slash)
    "<" <> rest -> #(rest, token.LessThan)
    ">" <> rest -> #(rest, token.GreaterThan)

    // Items
    "\"" <> rest -> {
      let #(rest2, contents) = read_string(rest, "")
      #(rest2, token.String(contents))
    }

    _ -> {
      let assert Ok(#(char, rest)) = string.pop_grapheme(source)

      case is_digit(char) {
        True -> {
          let #(rest2, integer) = read_int(rest, char)
          #(rest2, token.Int(integer))
        }

        False ->
          case is_letter(char) {
            True -> {
              let #(rest2, identifier) = read_ident(rest, char)
              #(rest2, token.lookup_ident(identifier))
            }

            False -> #(source, token.Illegal(char))
          }
      }
    }
  }
}

fn read_string(input, str) {
  case string.pop_grapheme(input) {
    Ok(#("\"", rest)) -> #(rest, str)
    Ok(#(char, rest)) -> read_string(rest, str <> char)
    Error(Nil) -> panic
  }
}

fn read_int(input, integer) {
  read_contents(input, integer, is_digit)
}

fn read_ident(input, str) {
  read_contents(input, str, is_letter)
}

fn read_contents(input, contents, predicate) {
  case string.pop_grapheme(input) {
    Ok(#(char, rest)) -> {
      case predicate(char) {
        True -> read_contents(rest, contents <> char, predicate)
        False -> #(input, contents)
      }
    }
    Error(Nil) -> #(input, contents)
  }
}

fn is_digit(str) {
  case str {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

fn is_letter(str) {
  case str {
    "a"
    | "b"
    | "c"
    | "d"
    | "e"
    | "f"
    | "g"
    | "h"
    | "i"
    | "j"
    | "k"
    | "l"
    | "m"
    | "n"
    | "o"
    | "p"
    | "q"
    | "r"
    | "s"
    | "t"
    | "u"
    | "v"
    | "w"
    | "x"
    | "y"
    | "z"
    | "A"
    | "B"
    | "C"
    | "D"
    | "E"
    | "F"
    | "G"
    | "H"
    | "I"
    | "J"
    | "K"
    | "L"
    | "M"
    | "N"
    | "O"
    | "P"
    | "Q"
    | "R"
    | "S"
    | "T"
    | "U"
    | "V"
    | "W"
    | "X"
    | "Y"
    | "Z"
    | "_" -> True
    _ -> False
  }
}

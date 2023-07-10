import Deez.Token
import Deez.Lexer

def repl: IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout

  repeat
    stdout.putStr ">> "
    stdout.flush
    let line ← stdin.getLine
    let mut l := Lexer.new line
    while l.ch != Char.ofNat 0 do
      let (l', tok) := l.nextToken
      l := l'
      stdout.putStrLn (tok |> toString)

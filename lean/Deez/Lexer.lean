import Deez.Token

structure Lexer :=
  input   : ByteArray := ByteArray.empty
  readPos : Nat       := 0
  pos     : Nat       := 0
  ch      : Char      := Char.ofNat 0
deriving Inhabited

namespace Lexer

def peek (l : Lexer) : Char :=
  let val := (if l.readPos >= l.input.size then 0 else l.input.get! l.readPos).toNat
  Char.ofNat val

def scanChar (l : Lexer) : Lexer :=
  { l with ch := l.peek, pos := l.readPos, readPos := l.readPos + 1 }

def new (input : String) : Lexer :=
  { input := input.toUTF8 } |> scanChar

partial def skipWhitespace (l : Lexer) : Lexer :=
  match l.ch.isWhitespace with
  | true => l.scanChar.skipWhitespace
  | false => l

partial def scanIdent (l : Lexer) (pos₀ : Nat) : Lexer × String :=
  match l.ch.isAlpha || l.ch = '_' with
  | true => l.scanChar.scanIdent pos₀
  | false => (l, String.fromUTF8Unchecked (l.input.extract pos₀ l.pos))

partial def scanInt (l : Lexer) (pos₀ : Nat) : Lexer × String :=
  match l.ch.isDigit with
  | true => l.scanChar.scanInt pos₀
  | false => (l, String.fromUTF8Unchecked (l.input.extract pos₀ l.pos))

def nextToken (l : Lexer) : Lexer × Token :=
  let l := l.skipWhitespace

  match l.ch with
  | '{' => (l.scanChar, .lBrace)
  | '}' => (l.scanChar, .rBrace)
  | '(' => (l.scanChar, .lParen)
  | ')' => (l.scanChar, .rParen)
  | ',' => (l.scanChar, .comma)
  | ';' => (l.scanChar, .semicolon)
  | '+' => (l.scanChar, .plus)
  | '-' => (l.scanChar, .minus)
  | '>' => (l.scanChar, .greaterThan)
  | '<' => (l.scanChar, .lessThan)
  | '*' => (l.scanChar, .asterisk)
  | '/' => (l.scanChar, .slash)
  | '\x00' => (l.scanChar, .eof)
  | '!' => if l.peek = '=' then (l.scanChar.scanChar, .notEqual) else (l.scanChar, .bang)
  | '=' => if l.peek = '=' then (l.scanChar.scanChar, .equal) else (l.scanChar, .assign)
  | _ => if l.ch.isAlpha || l.ch = '_' then
           let (l, str) := l.scanIdent l.pos
           (l, str.ofToken)
         else if l.ch.isDigit then
           let (l, str) := l.scanInt l.pos
           (l, .int str)
         else (l.scanChar, .illegal)

end Lexer

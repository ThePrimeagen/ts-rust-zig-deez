//> using lib "org.typelevel::toolkit::latest.release"
//> using option -Wunused:all

import cats.effect.IOApp
import repl.*
import cats.effect.std.Console
import cats.effect.IO

object Main extends IOApp.Simple:
  def run = start(Console[IO])

package object repl {
  import token.*
  import lexer.*
  import cats.effect.std.Console
  import cats.effect.IO

  val prompt = ">> "

  def start(console: Console[IO]): IO[Unit] =
    def printTokens(lexer: Lexer): IO[Unit] =
      val (nextLexer, token) = lexer.nextToken
      if (token != Token.EOF)
        console.println(token) *> printTokens(nextLexer)
      else
        console.println(token)

    def loop: IO[Unit] =
      for
        _ <- console.print(prompt)
        line <- console.readLine
        _ <- line.trim match
          case inputLine =>
            printTokens(Lexer(inputLine))
        _ <- loop
      yield ()

    loop
}

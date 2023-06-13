import ArgumentParser

struct LexerCli: ParsableCommand {
   public static let configuration = CommandConfiguration(
       abstract: "A Swift command-line tool to lex a string."
   )

    mutating func run() {
        print("main function needs to be implemented")
    }
}

LexerCli.main()

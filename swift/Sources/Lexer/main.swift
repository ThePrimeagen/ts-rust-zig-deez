import ArgumentParser

struct REPL: ParsableCommand {
    @Argument private var prompt = ">> "

    mutating func run() {
        print("Welcome to the Monkey language REPL")
        print("Enter 'quit' to exit")
        print(prompt, terminator: "")
        while let line = readLine(), line != "quit" {
            var lexer = Lexer(input: line)
            let tokens = lexer.evaluate()
            print(tokens)
            print(prompt, terminator: "")
        }
    }
}

REPL.main()

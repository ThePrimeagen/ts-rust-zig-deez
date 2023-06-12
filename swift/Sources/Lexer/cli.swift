import ArgumentParser

 @main
 public struct LexerCli: ParsableCommand {

     public static let configuration = CommandConfiguration(
         abstract: "A Swift command-line tool to lex a string."
     )

     public init() { }


     public mutating func run() throws {

         let identifier = ">>>"
         print(identifier)
         while let line = readLine() {
             var lexer = Lexer.from(code: line)
             var token = lexer.nextToken()

             while token != .eof {
                 print(token)
                 token = lexer.nextToken()
             }
             print(identifier)
         }

     }
 }
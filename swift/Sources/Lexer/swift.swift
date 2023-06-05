import ArgumentParser

@main
public struct LexerCli: ParsableCommand {

    public static let configuration = CommandConfiguration(
        abstract: "A Swift command-line tool to lex a string."
    )

    public init() { }
}

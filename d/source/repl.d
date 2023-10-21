/**
 * Basic REPL based on the awesome book
 * "Writing an Interpreter in Go" by Thorsten Ball.
 *
 * Authors: ToraKuma42
 * License: MIT
 * Version: 0.0.1
 */

import atom;
import evaluator;
import lexer;
import parser;
import deimos.linenoise;
import std.conv : to;
import std.format : format;
import std.range : enumerate;
import std.stdio : writefln;
import std.string : strip;

private static const string PROMPT = "Hello! This is the Monkey programming language!
Feel free to type in commands";

/**
 * Example repl for language.
 */
void repl()
{
    writefln("%s", PROMPT);
    linenoiseSetMultiLine(1); // FIXME: Might not work?

    char* currLine;
    auto env = new Environment();
    auto macroEnv = new Environment();

    // Context for REPL continuation
    auto prevLexer = Lexer("");
    auto prevParser = Parser(prevLexer);

    // Main REPL loop
    while ((currLine = linenoise(">> ")) !is null) {
        if (currLine[0] == '\0') {
            continue;
        }

        auto input = strip(to!string(currLine));
        if (input == "") {
            continue;
        }

        auto currLexer = Lexer((prevLexer.input != "") ? format("%s;%s",
                cast(string)(prevLexer.input), input) : input,
                prevLexer.position, prevLexer.tokens, prevLexer.endPosition);

        auto oldTokenCount = prevLexer.tokens.tag[].length;

        currLexer.tokenize();

        auto currParser = Parser(currLexer, oldTokenCount, prevParser.program);
        auto oldStatementCount = prevParser.program.statements[].length;

        currParser.parseProgram();

        // Retry if the current parser did not have any new statements
        auto statementCheckpoint = currParser.program.statements[].length;
        if (statementCheckpoint == oldStatementCount) {
            prevLexer.tokens.start.shrinkTo(oldTokenCount);
            prevLexer.tokens.tag.shrinkTo(oldTokenCount);

            prevParser.program.statements.shrinkTo(oldStatementCount);
            continue;
        }

        // Retry if the current parser encountered any errors
        if (currParser.errors[].length != 0) {
            writefln("Whoops! We ran into some monkey business here!\nParser errors:");

            foreach (error; currParser.errors[]) {
                writefln("\t%s", error);
            }

            prevLexer.tokens.start.shrinkTo(oldTokenCount);
            prevLexer.tokens.tag.shrinkTo(oldTokenCount);

            prevParser.program.statements.shrinkTo(oldStatementCount);
            continue;
        }

        auto evaluator = Evaluator(currParser, env, macroEnv, oldStatementCount);

        evaluator.defineMacros();
        evaluator.expandMacros();
        evaluator.evalProgram();

        auto result = evaluator.showResult();
        if (result != "") {
            writefln("%s", result);
        }

        prevLexer = currLexer;
        prevParser = currParser;
    }
}

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
import deimos.linenoise : linenoise;
import std.conv : to;
import std.range : enumerate;
import std.stdio : writefln;

private static const string PROMPT = "Hello! This is the Monkey programming language!
Feel free to type in commands";

/**
 * Example repl for language.
 */
void repl()
{
    writefln("%s", PROMPT);

    char* line;
    auto env = new Environment();

    while ((line = linenoise(">> ")) !is null)
    {
        if (line[0] != '\0')
        {
            auto lexer = Lexer(to!string(line));
            lexer.tokenize();

            auto parser = Parser(lexer);
            parser.parseProgram();

            if (parser.errors[].length != 0)
            {
                writefln("Whoops! We ran into some monkey business here!\nParser errors:");

                foreach (error; parser.errors[])
                {
                    writefln("\t%s", error);
                }

                continue;
            }

            auto evaluator = Evaluator(parser, env);

            evaluator.evalProgram();

            auto result = evaluator.showResult();
            if (result != "")
            {
                writefln("%s", result);
            }
        }
    }
}

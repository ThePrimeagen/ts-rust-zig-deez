/**
 * Basic REPL based on the awesome book
 * "Writing an Interpreter in Go" by Thorsten Ball.
 *
 * Authors: ToraKuma42
 * License: MIT
 * Version: 0.0.1
 */

import lexer;
import deimos.linenoise : linenoise;
import std.conv : to;
import std.range : enumerate;
import std.stdio : writefln;

/**
 * Example repl for language.
 */
void repl()
{

    char* line;

    while ((line = linenoise(">> ")) !is null)
    {
        if (line[0] != '\0')
        {
            auto lexer = Lexer(to!string(line));
            lexer.tokenize();

            foreach (i, tag; lexer.tokens.tag.opSlice().enumerate(0))
            {
                writefln("{Type:%s Literal:%s}", tag, lexer.tagRepr(i));
            }
        }
    }
}

using System.Diagnostics.Contracts;
using Monkey.Lexing;

namespace Monkey.Parsing;

// This is the exact same approach we took in ../Lexing/StringReader.cs
public readonly record struct TokenReader(IToken[] Tokens, int Position = 0)
{
    [Pure]
    public int Length => Tokens.Length - Position;

    [Pure]
    public IToken this[int i] => Tokens[Position + i];

    [Pure]
    public TokenReader Slice(int start, int length)
    {
        return Position + start + length == Tokens.Length
            ? this with { Position = Position + start }
            : new TokenReader(Tokens[(Position + start)..(Position + start + length)]);
    }

    [Pure]
    public TokenReader SkipWhile(Func<IToken, bool> predicate)
    {
        var i = Position;

        while (i < Tokens.Length && predicate(Tokens[i]))
        {
            i++;
        }

        return this with { Position = i };
    }
}
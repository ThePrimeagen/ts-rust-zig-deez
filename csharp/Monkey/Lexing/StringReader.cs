using System.Diagnostics.Contracts;

namespace Monkey.Lexing;

// This type supports custom list-based pattern matching by implementing the necessary methods based on a convention
public readonly record struct StringReader(string Input, int Position = 0)
{
    [Pure]
    // The `Length` property is necessary for list pattern matching.
    // We can also use `Count()` method instead
    public int Length => Input.Length - Position;

    [Pure]
    // This allows us to use index-based list pattern matching like `['c']` or `['c','d', ..]`
    public char this[int i] => Input[Position + i];

    [Pure]
    // This allows us to use range-based pattern matching like `['c', .. var rest]`
    // This can also be declared as indexer with a `Range` parameter: `public StringReader this[Range range] => ...;`
    public StringReader Slice(int start, int length)
    {
        return Position + start + length == Input.Length
            ? this with { Position = Position + start }
            : new StringReader(Input[(Position + start)..(Position + start + length)]);
    }

    [Pure]
    public (string Result, StringReader Remaining) ReadWhile(Func<char, bool> predicate)
    {
        var remaining = SkipWhile(predicate);

        return (Input[Position..remaining.Position], remaining);
    }

    [Pure]
    public StringReader SkipWhile(Func<char, bool> predicate)
    {
        var i = Position;

        while (i < Input.Length && predicate(Input[i]))
        {
            i++;
        }

        return this with { Position = i };
    }

    [Pure]
    public override string ToString() => Input[Position..];
}
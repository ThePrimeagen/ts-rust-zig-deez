/**
 * Object atom structures based on the awesome book
 * "Writing an Interpreter in Go" by Thorsten Ball.
 *
 * Authors: ToraKuma42
 * License: MIT
 * Version: 0.0.1
 */

import std.format : format;
import std.sumtype;

/// Original source of expressions: https://dlang.org/library/std/sumtype.html
/// Most fundamental type of "nothing" according to homotopy type theory
struct Unit
{
}

/// Wraps evaluation results
alias EvalResult = SumType!(long, bool, string, Unit);
alias LongResult = EvalResult.Types[0];
alias BoolResult = EvalResult.Types[1];
alias StringResult = EvalResult.Types[2];
alias VoidResult = EvalResult.Types[3];

/// Describe type of atom for inspection
enum AtomType : ubyte
{
    Null,
    Integer,
    Boolean,
    Return,
    Error
}

static const NULL_ATOM = new NullAtom(); /// Instance of null atom
static const TRUE_ATOM = new BooleanAtom(true); /// Instance of true value
static const FALSE_ATOM = new BooleanAtom(false); /// Instance of false value

/// Most fundamental object for evaluation
/// TODO: use emplace for custom class allocation
interface Atom
{
    /**
     * Show the underlying type of the atom.
     * Returns: The atom type
     */
    AtomType type();

    /**
     * Show the underlying value of the atom.
     * Returns: the atom value
     */
    string inspect();
}

/// Evaluation value of null
/// Do not create directly: Use NULL_ATOM
final class NullAtom : Atom
{
    /// Describe null atom type
    AtomType type()
    {
        return AtomType.Null;
    }

    /// Write value of null atom
    string inspect()
    {
        return "null";
    }
}

/// Show error in value form
class ErrorAtom : Atom
{
    string val; /// Underlying error message

    /**
     * Construct the error atom
     * Params: val = the message value to wrap
     */
    this(string val)
    {
        this.val = val;
    }

    /// Describe integer atom type
    AtomType type()
    {
        return AtomType.Error;
    }

    /// Write value of error atom
    string inspect()
    {
        return format("ERROR: %s", this.val);
    }
}

/// Evaluation value in integer form
class IntegerAtom : Atom
{
    long val; /// Underlying integer value

    /**
     * Construct the integer atom
     * Params: val = the integer value to wrap
     */
    this(long val)
    {
        this.val = val;
    }

    /// Describe integer atom type
    AtomType type()
    {
        return AtomType.Integer;
    }

    /// Write value of integer atom
    string inspect()
    {
        return format("%d", this.val);
    }
}

/// Evaluation value in boolean form
/// Do not create directly: Use TRUE_ATOM and FALSE_ATOM
class BooleanAtom : Atom
{
    bool val; /// Underlying boolean value

    /**
     * Construct the boolean atom
     * Params: val = the boolean value to wrap
     */
    this(bool val)
    {
        this.val = val;
    }

    /// Describe boolean atom type
    AtomType type()
    {
        return AtomType.Boolean;
    }

    /// Write value of boolean atom
    string inspect()
    {
        return format("%s", this.val);
    }
}

/// Evaluation return value
class ReturnAtom : Atom
{
    Atom val; /// Underlying return value

    /**
     * Construct the return atom
     * Params: val = the return value to wrap
     */
    this(Atom val)
    {
        this.val = val;
    }

    /// Describe return atom type
    AtomType type()
    {
        return AtomType.Return;
    }

    /// Write value of return atom
    string inspect()
    {
        return format("%s", this.val);
    }
}

/// Creates new sibling environment referencing parent environment
/// Very useful for block statements
Environment* newEnclosedEnvironment(Environment* outer)
{
    auto env = new Environment();
    env.outer = outer;
    return env;
}

/// Map out functions and variables in closures
struct Environment
{
    Environment* outer; /// Parent environment
    EvalResult[string] items; /// items in hashmap
}

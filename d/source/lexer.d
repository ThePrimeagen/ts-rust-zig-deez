import std.stdio;

void main()
{
    writeln("Edit source/app.d to start your project.");

}

// Primary lexer test
unittest
{
    const string input = "
  let five = 5;
  let ten = 10;
  let add = fn(x, y) {
    x + y;
  };
  let result = add(five, ten);
  ";

    // TODO: add expected output
}

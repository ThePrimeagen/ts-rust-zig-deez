<?php

require 'vendor/autoload.php';

enum Precedence: int
{
    case Lowest = 1;
    case Equals = 2;        // ==
    case LessGreater = 3;   // > or <
    case Sum = 4;           // +
    case Product = 5;       // *
    case Prefix = 6;        // -X or !X
    case Call = 7;          // myFunction(X)
}

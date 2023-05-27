<?php

require_once "Tokenizer.php";
require_once "Parser.php";

$parser = new Parser(new Tokenizer("10000000000000000000000000 <   0000001 + 5"));
$program = $parser->parseProgram();

$parser = new Parser(new Tokenizer("!true + 3 * (3 + 3) + -----false * !0"));
$program = $parser->parseProgram();

echo $program;


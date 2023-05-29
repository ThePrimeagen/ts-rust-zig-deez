<?php

require_once "Tokenizer.php";
require_once "Parser.php";

$parser = new Parser(new Tokenizer("10000000000000000000000000 <   0000001 + 5"));
$program = $parser->parseProgram();
echo $program, PHP_EOL, PHP_EOL;

$parser = new Parser(new Tokenizer("!true + 3 * (3 + 3) + -----false * !0"));
$program = $parser->parseProgram();
echo $program, PHP_EOL, PHP_EOL;

$parser = new Parser(new Tokenizer("if (x < y) {x} else {if (y > x) {y}}"));
$program = $parser->parseProgram();
echo $program, PHP_EOL, PHP_EOL;

$parser = new Parser(new Tokenizer("fn (x, y) {x + y}"));
$program = $parser->parseProgram();
echo $program, PHP_EOL, PHP_EOL;

$parser = new Parser(new Tokenizer("add(1, 2+3, 4 + 5)"));
$program = $parser->parseProgram();
echo $program, PHP_EOL, PHP_EOL;

$parser = new Parser(new Tokenizer("add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"));
$program = $parser->parseProgram();
echo $program, PHP_EOL, PHP_EOL;

$parser = new Parser(new Tokenizer("return(true(10));"));
$program = $parser->parseProgram();
echo $program, PHP_EOL, PHP_EOL;

$parser = new Parser(new Tokenizer("let f = 10;"));
$program = $parser->parseProgram();
echo $program, PHP_EOL, PHP_EOL;

$parser = new Parser(new Tokenizer("true"));
$program = $parser->parseProgram();
echo $program, PHP_EOL, PHP_EOL;

$parser = new Parser(new Tokenizer("true == false"));
$program = $parser->parseProgram();
echo $program, PHP_EOL, PHP_EOL;

$parser = new Parser(new Tokenizer("if(true){x}"));
$program = $parser->parseProgram();
echo $program, PHP_EOL, PHP_EOL;

$parser = new Parser(new Tokenizer('"hello\"\t"'));
$program = $parser->parseProgram();
echo $program, PHP_EOL, PHP_EOL;

$parser = new Parser(new Tokenizer('"hello I am\"unterminated'));
$program = $parser->parseProgram();
var_dump($parser->getErrors());
echo $program, PHP_EOL, PHP_EOL;

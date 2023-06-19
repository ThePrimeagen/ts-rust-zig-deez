<?php

require_once __DIR__ . "/vendor/autoload.php";

$file = $argv[1] ?? null;

if ($file === null) {
    require "repl.php";
    exit;
}

function printParserErrors(array $errors): void {
    echo "Parser errors:", PHP_EOL;
    foreach ($errors as $error) {
        echo "\t", $error, PHP_EOL;
    }
}

$fileContent = file_get_contents($file);
$globalScope = new Scope();
$tokenizer = new Tokenizer($fileContent);
$parser = new Parser($tokenizer);
$program = $parser->parseProgram();
$errors = $parser->getErrors();

if (count($errors) > 0) {
    printParserErrors($errors);
    exit;
}

$evaluator = new Evaluator();
$result = $evaluator->evaluate($program, $globalScope);

if ($result !== null) {
    echo $result->inspect(), PHP_EOL;
} else {
    echo "null", PHP_EOL;
}

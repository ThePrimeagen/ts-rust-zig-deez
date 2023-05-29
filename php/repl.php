<?php

require_once "Parser.php";
require_once "Tokenizer.php";
require_once "Evaluator.php";
require_once "Scope.php";

function printParserErrors(array $errors): void {
    echo "Parser errors:", PHP_EOL;
    foreach ($errors as $error) {
        echo "\t", $error, PHP_EOL;
    }
}

$globalScope = new Scope();

while (420 > 69) {
    $input = readline(">> ");

    if (trim($input) === "") {
        continue;
    }

    $tokenizer = new Tokenizer($input);
    $parser = new Parser($tokenizer);

    $program = $parser->parseProgram();
    $errors = $parser->getErrors();
    if (count($errors) > 0) {
        printParserErrors($errors);
        continue;
    }

    $evaluator = new Evaluator();
    $result = $evaluator->evaluate($program, $globalScope);

    if ($result !== null) {
        echo $result->inspect(), PHP_EOL;
    } else {
        echo "null", PHP_EOL;
    }
}

<?php

require_once "Tokenizer.php";

while (420 > 69) {
    $input = readline(">> ");

    $tokenizer = new Tokenizer($input);

    do {
        $token = $tokenizer->getNextToken();
        echo $token->type->name, " :: ", $token->literal, PHP_EOL;
    } while ($token->type !== TokenType::Eof);
}

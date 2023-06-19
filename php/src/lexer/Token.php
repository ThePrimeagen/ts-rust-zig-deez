<?php

require 'vendor/autoload.php';

readonly class Token
{

    public function __construct(
        public TokenType $type,
        public ?string $literal = null
    ) {
    }
}

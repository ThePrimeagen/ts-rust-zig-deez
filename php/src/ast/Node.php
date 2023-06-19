<?php

require 'vendor/autoload.php';

interface Node extends Stringable {
    public function __toString(): string;

    public function tokenLiteral(): string;
}

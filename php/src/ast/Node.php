<?php

require 'vendor/autoload.php';

interface Node extends Stringable
{
    public function tokenLiteral(): string;
    public function __toString(): string;
}
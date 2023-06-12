<?php

require 'vendor/autoload.php';

interface Statement extends Node
{
    public function statementNode(): void;
}

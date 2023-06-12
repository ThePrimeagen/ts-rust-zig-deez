<?php

require 'vendor/autoload.php';

interface Expression extends Node
{
    public function expressionNode(): void;
}
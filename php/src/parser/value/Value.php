<?php

require 'vendor/autoload.php';

interface Value
{
    public function type(): string;
    public function inspect(): string;
}

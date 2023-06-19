<?php

require 'vendor/autoload.php';

readonly class StringValue implements Value
{
    public function __construct(public string $value)
    {
    }

    public function type(): string
    {
        return "STRING";
    }

    public function inspect(): string
    {
        return $this->value;
    }
}

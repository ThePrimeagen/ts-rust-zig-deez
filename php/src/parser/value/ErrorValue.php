<?php

require 'vendor/autoload.php';

readonly class ErrorValue implements Value
{

    public function __construct(public string $message)
    {
    }

    public function type(): string
    {
        return "ERROR";
    }

    public function inspect(): string
    {
        return "ERROR: " . $this->message;
    }
}

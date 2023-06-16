<?php

require 'vendor/autoload.php';

readonly class ReturnValue implements Value
{
    public function __construct(public Value $value)
    {
    }

    public static function unwrap(Value $value)
    {
        if ($value instanceof ReturnValue) {
            return $value->value;
        }

        return $value;
    }

    public function type(): string
    {
        return "RETURN_VALUE";
    }

    public function inspect(): string
    {
        return $this->value->inspect();
    }
}

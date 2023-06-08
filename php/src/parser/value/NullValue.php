<?php

require 'vendor/autoload.php';

class NullValue implements Value
{
    public static NullValue $NULL;

    public static function init(): void
    {
        self::$NULL = new NullValue();
    }

    private function __construct()
    {
    }

    public function type(): string
    {
        return "NULL";
    }

    public function inspect(): string
    {
        return "null";
    }
}

NullValue::init();

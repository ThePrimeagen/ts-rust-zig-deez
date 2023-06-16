<?php

require 'vendor/autoload.php';

class NullValue implements Value {
    public static NullValue $NULL;

    private function __construct() {
    }

    public static function init(): void {
        self::$NULL = new NullValue();
    }

    public function type(): string {
        return 'NULL';
    }

    public function inspect(): string {
        return 'null';
    }
}

NullValue::init();

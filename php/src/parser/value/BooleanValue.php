<?php

require 'vendor/autoload.php';

class BooleanValue implements Value
{
    public static BooleanValue $TRUE;
    public static BooleanValue $FALSE;

    public static function init(): void
    {
        self::$TRUE = new BooleanValue(true);
        self::$FALSE = new BooleanValue(false);
    }

    private function __construct(public bool $value)
    {
    }

    public static function fromBool(bool $value): BooleanValue
    {
        return $value ? self::$TRUE : self::$FALSE;
    }

    public static function isTruthy(?Value $conditionTrue): bool
    {
        return match ($conditionTrue) {
            self::$FALSE, NullValue::$NULL => false,
            default => true,
        };
    }

    public function type(): string
    {
        return "BOOLEAN";
    }

    public function inspect(): string
    {
        return $this->value ? "true" : "false";
    }
}

BooleanValue::init();

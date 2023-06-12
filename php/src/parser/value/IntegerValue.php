<?php

require 'vendor/autoload.php';

readonly class IntegerValue implements Value
{
    public function __construct(public int $value)
    {
    }

    public function type(): string
    {
        return "INTEGER";
    }

    public function inspect(): string
    {
        return (string) $this->value;
    }

    public static function add(int $left, int $right): IntegerValue | ErrorValue
    {
        if ($left > 0 && PHP_INT_MAX - $left < $right) {
            return new ErrorValue("Integer overflow: $left + $right");
        }

        if ($left < 0 && PHP_INT_MIN - $left > $right) {
            return new ErrorValue("Integer underflow: $left + $right");
        }

        return new IntegerValue($left + $right);
    }

    public static function sub(int $left, int $right): IntegerValue | ErrorValue
    {
        if ($right === PHP_INT_MIN) {
            if ($left >= 0) {
                return new ErrorValue("Integer overflow: $left - $right");
            }

            return new IntegerValue(($left + 1) + PHP_INT_MAX);
        }

        $inverse = -$right;

        if ($left > 0 && PHP_INT_MAX - $left < $inverse) {
            return new ErrorValue("Integer overflow: $left - $right");
        }

        if ($left < 0 && PHP_INT_MIN - $left > $inverse) {
            return new ErrorValue("Integer underflow: $left - $right");
        }

        return new IntegerValue($left + $inverse);
    }

    public static function mul(int $left, int $right): IntegerValue | ErrorValue
    {
        $product = $left * $right;

        if ($product > PHP_INT_MAX) {
            return new ErrorValue("Integer overflow: $left * $right");
        }

        if ($product < PHP_INT_MIN) {
            return new ErrorValue("Integer underflow: $left * $right");
        }

        return new IntegerValue($product);
    }

    public static function div(int $left, int $right): IntegerValue | ErrorValue
    {
        try {
            return new IntegerValue(intdiv($left, $right));
        } catch (DivisionByZeroError $e) {
            return new ErrorValue("Division by zero: $left / $right");
        } catch (ArithmeticError $e) {
            return new ErrorValue("Integer overflow: $left / $right");
        }
    }
}

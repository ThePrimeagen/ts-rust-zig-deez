<?php

require_once "Scope.php";

interface Value {
    public function type(): string;
    public function inspect(): string;
}

readonly class IntegerValue implements Value {
    public function __construct(public int $value) {
    }

    public function type(): string {
        return "INTEGER";
    }

    public function inspect(): string {
        return (string) $this->value;
    }

    public static function add(int $left, int $right): IntegerValue | ErrorValue {
        if ($left > 0 && PHP_INT_MAX - $left < $right) {
            return new ErrorValue("Integer overflow: $left + $right");
        }

        if ($left < 0 && PHP_INT_MIN - $left > $right) {
            return new ErrorValue("Integer underflow: $left + $right");
        }

        return new IntegerValue($left + $right);
    }

    public static function sub(int $left, int $right): IntegerValue | ErrorValue {
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

    public static function mul(int $left, int $right): IntegerValue | ErrorValue {
        $product = $left * $right;

        if ($product > PHP_INT_MAX) {
            return new ErrorValue("Integer overflow: $left * $right");
        }

        if ($product < PHP_INT_MIN) {
            return new ErrorValue("Integer underflow: $left * $right");
        }

        return new IntegerValue($product);
    }

    public static function div(int $left, int $right): IntegerValue | ErrorValue {
        try {
            return new IntegerValue(intdiv($left, $right));
        } catch (DivisionByZeroError $e) {
            return new ErrorValue("Division by zero: $left / $right");
        } catch (ArithmeticError $e) {
            return new ErrorValue("Integer overflow: $left / $right");
        }
    }
}

class BooleanValue implements Value {
    public static BooleanValue $TRUE;
    public static BooleanValue $FALSE;

    public static function init(): void {
        self::$TRUE = new BooleanValue(true);
        self::$FALSE = new BooleanValue(false);
    }

    private function __construct(public bool $value) {
    }

    public static function fromBool(bool $value): BooleanValue {
        return $value ? self::$TRUE : self::$FALSE;
    }

    public static function isTruthy(?Value $conditionTrue): bool {
        return match ($conditionTrue) {
            self::$FALSE, NullValue::$NULL => false,
            default => true,
        };
    }

    public function type(): string {
        return "BOOLEAN";
    }

    public function inspect(): string {
        return $this->value ? "true" : "false";
    }
}

BooleanValue::init();

class NullValue implements Value {
    public static NullValue $NULL;

    public static function init(): void {
        self::$NULL = new NullValue();
    }

    private function __construct() {}

    public function type(): string {
        return "NULL";
    }

    public function inspect(): string {
        return "null";
    }
}

NullValue::init();

readonly class ReturnValue implements Value {
    public function __construct(public Value $value) {
    }

    public static function unwrap(Value $value) {
        if ($value instanceof ReturnValue) {
            return $value->value;
        }

        return $value;
    }

    public function type(): string {
        return "RETURN_VALUE";
    }

    public function inspect(): string {
        return $this->value->inspect();
    }
}

readonly class ErrorValue implements Value {

    public function __construct(public string $message) {
    }

    public function type(): string {
        return "ERROR";
    }

    public function inspect(): string {
        return "ERROR: " . $this->message;
    }
}

readonly class FunctionValue implements Value {

    /**
     * @param Identifier[] $parameters
     * @param BlockStatement $body
     * @param Scope $scope
     */
    public function __construct(public array $parameters,
                                public BlockStatement $body,
                                public Scope $scope) {
    }

    public function type(): string {
        return "FUNCTION";
    }

    public function inspect(): string {
        return "fn(" . implode(", ", $this->parameters) . ") {\n" . $this->body . "\n}";
    }

    public function createChildScope(array $arguments): Scope {
        $childScope = new Scope($this->scope);

        foreach ($this->parameters as $index => $parameter) {
            $childScope->set($parameter->value, $arguments[$index]);
        }

        return $childScope;
    }
}

readonly class StringValue implements Value {
    public function __construct(public string $value) {
    }

    public function type(): string {
        return "STRING";
    }

    public function inspect(): string {
        return $this->value;
    }
}

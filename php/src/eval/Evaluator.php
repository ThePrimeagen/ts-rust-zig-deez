<?php

readonly class Evaluator {

    private BuiltinFunctions $builtinFunctions;

    public function __construct() {
        $this->builtinFunctions = new BuiltinFunctions();
    }

    public function evaluate(Node $node, Scope $scope): ?Value {
        return match (true) {
            // Statements
            $node instanceof Program => $this->evaluateProgram($node, $scope),
            $node instanceof ExpressionStatement => $this->evaluate($node->expression, $scope),

            // Literal Expressions
            $node instanceof IntegerLiteral => new IntegerValue($node->value),
            $node instanceof BooleanLiteral => BooleanValue::fromBool($node->value),
            $node instanceof StringLiteral => new StringValue($node->value),

            // Computed Expressions
            $node instanceof PrefixExpression => $this->evaluatePrefixExpression($node, $scope),
            $node instanceof InfixExpression => $this->evaluateInfixExpression($node, $scope),
            $node instanceof BlockStatement => $this->evaluateBlockStatement($node, $scope),
            $node instanceof IfExpression => $this->evaluateIfExpression($node, $scope),
            $node instanceof ReturnStatement => $this->evaluateReturnStatement($node, $scope),
            $node instanceof LetStatement => $this->evaluateLetStatement($node, $scope),
            $node instanceof Identifier => $this->evaluateIdentifier($node, $scope),
            $node instanceof FunctionLiteral => $this->evaluateFunctionLiteral($node, $scope),
            $node instanceof CallExpression => $this->evaluateCallExpression($node, $scope),
            $node instanceof ArrayLiteral => $this->evaluateArrayLiteral($node, $scope),
            $node instanceof IndexExpression => $this->evaluateIndexExpression($node, $scope),
            $node instanceof HashLiteral => $this->evaluateHashLiteral($node, $scope),

            default => null,
        };
    }

    private function evaluateProgram(Program $program, Scope $scope): ?Value {
        $result = null;

        foreach ($program->statements as $statement) {
            $result = $this->evaluate($statement, $scope);

            if ($result instanceof ReturnValue) {
                return $result->value;
            }

            if ($result instanceof ErrorValue) {
                return $result;
            }
        }

        return $result;
    }

    private function evaluateBlockStatement(BlockStatement $node, Scope $scope): ?Value {
        $result = null;

        foreach ($node->statements as $statement) {
            $result = $this->evaluate($statement, $scope);

            if ($result instanceof ReturnValue || $result instanceof ErrorValue) {
                return $result;
            }
        }

        return $result;
    }

    private function evaluateIfExpression(IfExpression $node, Scope $scope): Value {
        $conditionValue = $this->evaluate($node->condition, $scope);

        if ($conditionValue instanceof ErrorValue) {
            return $conditionValue;
        }

        if (BooleanValue::isTruthy($conditionValue)) {
            return $this->evaluate($node->consequence, $scope);
        }

        if ($node->alternative !== null) {
            return $this->evaluate($node->alternative, $scope);
        }

        return NullValue::$NULL;
    }

    private function evaluateReturnStatement(ReturnStatement $node, Scope $scope): Value {
        $value = $this->evaluate($node->returnValue, $scope);

        if ($value instanceof ErrorValue) {
            return $value;
        }

        return new ReturnValue($value);
    }

    private function evaluateLetStatement(LetStatement $node, Scope $scope): Value {
        $value = $this->evaluate($node->value, $scope);

        if ($value instanceof ErrorValue) {
            return $value;
        }

        $scope->set($node->identifier->value, $value);

        return $value;
    }

    private function evaluatePrefixExpression(PrefixExpression $node, Scope $scope): Value {
        $rightValue = $this->evaluate($node->right, $scope);

        if ($rightValue instanceof ErrorValue) {
            return $rightValue;
        }

        return match ($node->token->type) {
            TokenType::Not => $this->evaluateNotOperatorExpression($rightValue),
            TokenType::Minus => $this->evaluateMinusOperatorExpression($rightValue),
            default => new ErrorValue("Unknown operator: {$node->token->type->name}{$node->right->token->type->name}"),
        };
    }

    private function evaluateInfixExpression(InfixExpression $node, Scope $scope): Value {
        $left = $this->evaluate($node->left, $scope);

        if ($left instanceof ErrorValue) {
            return $left;
        }

        $right = $this->evaluate($node->right, $scope);

        if ($right instanceof ErrorValue) {
            return $right;
        }

        if (!($left instanceof $right)) {
            return new ErrorValue("Type mismatch: {$left->type()} {$node->token->type->name} {$right->type()}");
        }

        if ($left instanceof IntegerValue) {
            /** @var IntegerValue $right */
            return $this->evaluateIntegerInfixExpression($node->token->type, $left, $right);
        }

        if ($left instanceof BooleanValue) {
            /** @var BooleanValue $right */
            return $this->evaluateBooleanInfixExpression($node->token->type, $left, $right);
        }

        if ($left instanceof StringValue) {
            /** @var StringValue $right */

            return $this->evaluateStringInfixExpression($node->token->type, $left, $right);
        }

        return new ErrorValue("Unknown operator: {$left->type()} {$node->token->type->name} {$right->type()}");
    }

    private function evaluateNotOperatorExpression(Value $value): Value {
        return match (true) {
            $value === BooleanValue::$TRUE => BooleanValue::$FALSE,
            $value === BooleanValue::$FALSE/*, $value === NullValue::$NULL*/ => BooleanValue::$TRUE,
            //            $value instanceof IntegerValue => $value->value === 0 ? BooleanValue::$TRUE : BooleanValue::$FALSE,
            default => new ErrorValue("Unknown operator: !{$value->type()}"),
        };
    }

    private function evaluateMinusOperatorExpression(Value $value): Value {
        if ($value instanceof IntegerValue) {
            if ($value->value === PHP_INT_MIN) {
                return new ErrorValue("Integer overflow: -{$value->value}");
            }

            return new IntegerValue(-$value->value);
        }

        return new ErrorValue("Unknown operator: -{$value->type()}");
    }

    private function evaluateIntegerInfixExpression(TokenType $type, IntegerValue $left, IntegerValue $right): Value {
        return match ($type) {
            // This is unsafe in PHP, as integers do not just overflow, they become floats.
            // See InterValue::add(), InterValue::subtract(), etc. for a safe implementation.
            //            TokenType::Plus => new IntegerValue($left->value + $right->value),
            //            TokenType::Minus => new IntegerValue($left->value - $right->value),
            //            TokenType::Asterisk => new IntegerValue($left->value * $right->value),
            //            TokenType::Slash => new IntegerValue((int) ($left->value / $right->value)),

            TokenType::Plus => IntegerValue::add($left->value, $right->value),
            TokenType::Minus => IntegerValue::sub($left->value, $right->value),
            TokenType::Asterisk => IntegerValue::mul($left->value, $right->value),
            TokenType::Slash => IntegerValue::div($left->value, $right->value),


            TokenType::LessThan => BooleanValue::fromBool($left->value < $right->value),
            TokenType::GreaterThan => BooleanValue::fromBool($left->value > $right->value),
            TokenType::Equals => BooleanValue::fromBool($left->value === $right->value),
            TokenType::NotEquals => BooleanValue::fromBool($left->value !== $right->value),
            default => new ErrorValue("Unknown operator: {$left->type()} {$type->name} {$right->type()}"),
        };
    }

    private function evaluateBooleanInfixExpression(TokenType $type, BooleanValue $left, BooleanValue $right): BooleanValue|ErrorValue {
        return match ($type) {
            TokenType::Equals => BooleanValue::fromBool($left === $right),
            TokenType::NotEquals => BooleanValue::fromBool($left !== $right),
            default => new ErrorValue("Unknown operator: {$left->type()} {$type->name} {$right->type()}"),
        };
    }

    private function evaluateStringInfixExpression(TokenType $type, StringValue $left, StringValue $right): StringValue|ErrorValue {
        return match ($type) {
            TokenType::Plus => new StringValue($left->value . $right->value),
            TokenType::Equals => BooleanValue::fromBool($left->value === $right->value),
            TokenType::NotEquals => BooleanValue::fromBool($left->value !== $right->value),
            default => new ErrorValue("Unknown operator: {$left->type()} {$type->name} {$right->type()}"),
        };
    }

    private function evaluateIdentifier(Identifier $node, Scope $scope): Value {
        $value = $scope->get($node->value);

        if ($value === null) {
            if (isset($this->builtinFunctions->functions[$node->value])) {
                return $this->builtinFunctions->functions[$node->value];
            }

            return new ErrorValue("Identifier not found: {$node->value}");
        }

        return $value;
    }

    private function evaluateFunctionLiteral(FunctionLiteral $node, Scope $scope): FunctionValue {
        return new FunctionValue($node->parameters, $node->body, $scope);
    }

    private function evaluateCallExpression(CallExpression $node, Scope $scope): Value {
        $function = $this->evaluate($node->function, $scope);

        if ($function instanceof ErrorValue) {
            return $function;
        }

        $arguments = $this->evaluateExpressions($node->arguments, $scope);

        if (count($arguments) === 1 && $arguments[0] instanceof ErrorValue) {
            return $arguments[0];
        }

        return $this->applyFunction($function, $arguments);
    }

    /**
     * @param Value $function
     * @param Value[] $arguments
     * @return Value
     */
    private function applyFunction(Value $function, array $arguments): Value {
        if ($function instanceof FunctionValue) {
            $childScope = $function->createChildScope($arguments);
            $value = $this->evaluate($function->body, $childScope);
            return ReturnValue::unwrap($value);
        }

        if ($function instanceof BuiltinFunctionValue) {
            return $function->function->call($function, $arguments);
        }

        return new ErrorValue("Not a function: {$function->type()}");
    }

    /**
     * @param array $arguments
     * @param Scope $scope
     * @return Value[]
     */
    private function evaluateExpressions(array $arguments, Scope $scope): array {
        $result = [];

        foreach ($arguments as $argument) {
            $value = $this->evaluate($argument, $scope);

            if ($value instanceof ErrorValue) {
                return [$value];
            }

            $result[] = $value;
        }

        return $result;
    }

    private function evaluateArrayLiteral(ArrayLiteral $node, Scope $scope): ArrayValue | ErrorValue {
        $elements = $this->evaluateExpressions($node->elements, $scope);

        if (count($elements) === 1 && $elements[0] instanceof ErrorValue) {
            return $elements[0];
        }

        return new ArrayValue($elements);
    }

    private function evaluateIndexExpression(IndexExpression $node, Scope $scope) {
        $left = $this->evaluate($node->left, $scope);

        if ($left instanceof ErrorValue) {
            return $left;
        }

        $index = $this->evaluate($node->index, $scope);

        if ($index instanceof ErrorValue) {
            return $index;
        }

        return $this->evaluateIndexValue($left, $index);
    }

    private function evaluateIndexValue(Value $container, Value $index): Value {
        if ($container instanceof ArrayValue && $index instanceof IntegerValue) {
            return $this->evaluateArrayIndexExpression($container, $index);
        }

        if ($container instanceof HashValue) {
            return $this->evaluateHashIndexExpression($container, $index);
        }

        return new ErrorValue("Index operator not supported: {$container->type()}");
    }

    private function evaluateArrayIndexExpression(ArrayValue $container, IntegerValue $index): Value {
        $array = $container->elements;
        $idx = $index->value;

        $element = $array[$idx] ?? null;

        if ($element === null) {
            return NullValue::$NULL;
        }

        return $array[$idx];
    }

    private function evaluateHashLiteral(HashLiteral $node, Scope $scope): HashValue | ErrorValue {
        $stringKeyPairs = null;
        $intKeyPairs = null;
        $true = null;
        $false = null;

        /** @var Expression $key */
        foreach ($node->pairs as $key) {
            /** @var Expression $value */
            $value = $node->pairs[$key];
            $key = $this->evaluate($key, $scope);

            if ($key instanceof ErrorValue) {
                return $key;
            }

            $value = $this->evaluate($value, $scope);

            if ($value instanceof ErrorValue) {
                return $value;
            }

            if ($key instanceof StringValue) {
                if ($stringKeyPairs === null) {
                    $stringKeyPairs = [];
                }
                $stringKeyPairs[$key->value] = $value;
                continue;
            }

            if ($key instanceof IntegerValue) {
                if ($intKeyPairs === null) {
                    $intKeyPairs = [];
                }
                $intKeyPairs[$key->value] = $value;
                continue;
            }

            if ($key === BooleanValue::$TRUE) {
                $true = $value;
                continue;
            }

            if ($key === BooleanValue::$FALSE) {
                $false = $value;
            }
        }

        return new HashValue($stringKeyPairs, $intKeyPairs, $true, $false);
    }

    private function evaluateHashIndexExpression(HashValue $container, Value $index): ?Value {
        return $container->lookup($index);
    }
}

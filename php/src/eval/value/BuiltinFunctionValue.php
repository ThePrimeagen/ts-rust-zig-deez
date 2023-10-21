<?php

readonly class BuiltinFunctionValue implements Value {

    /**
     * @param Closure $function
     */
    public function __construct(
        public Closure $function,
    ) {
    }

    public function type(): string {
        return "BUILTIN_FUNCTION";
    }

    public function inspect(): string {
        return "builtin function";
    }
}

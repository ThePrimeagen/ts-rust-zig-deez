<?php

require 'vendor/autoload.php';

readonly class FunctionValue implements Value
{

    /**
     * @param Identifier[] $parameters
     * @param BlockStatement $body
     * @param Scope $scope
     */
    public function __construct(
        public array $parameters,
        public BlockStatement $body,
        public Scope $scope
    ) {
    }

    public function type(): string
    {
        return "FUNCTION";
    }

    public function inspect(): string
    {
        return "fn(" . implode(", ", $this->parameters) . ") {\n" . $this->body . "\n}";
    }

    public function createChildScope(array $arguments): Scope
    {
        $childScope = new Scope($this->scope);

        foreach ($this->parameters as $index => $parameter) {
            $childScope->set($parameter->value, $arguments[$index]);
        }

        return $childScope;
    }
}

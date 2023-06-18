<?php

readonly class ArrayValue implements Value {

    /**
     * @param Value[] $elements
     */
    public function __construct(public array $elements) {
    }

    public function type(): string {
        return "ARRAY";
    }

    public function inspect(): string {
        $elements = array_map(function ($e) {
            return $e->inspect();
        }, $this->elements);
        return "[" . implode(", ", $elements) . "]";
    }
}

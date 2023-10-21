<?php

class Scope {
    private array $store = [];
    private ?Scope $outer;

    public function __construct(?Scope $outer = null) {
        $this->outer = $outer;
    }

    public function get(string $name): ?Value {
        $value = $this->store[$name] ?? null;

        if ($value === null && $this->outer !== null) {
            $value = $this->outer->get($name);
        }

        return $value;
    }

    public function set(string $name, Value $value): Value {
        $this->store[$name] = $value;
        return $value;
    }

    public function reassign(string $name, Value $value): ?Value {
        if (isset($this->store[$name])) {
            $this->store[$name] = $value;
            return $value;
        }

        if (isset($this->outer)) {
            return $this->outer->reassign($name, $value);
        } else {
            return null;
        }
    }
}

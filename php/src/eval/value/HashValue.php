<?php

class HashValue implements Value {

    /**
     * @param Value[] $stringKeyPairs
     * @param Value[] $intKeyPairs
     * @param Value|null $true
     * @param Value|null $false
     */
    public function __construct(
        public ?array $stringKeyPairs,
        public ?array $intKeyPairs,
        public ?Value $true,
        public ?Value $false,
    ) {
    }

    public function type(): string {
        return "HASH";
    }

    public function inspect(): string {
        $pairs = [];

        if ($this->stringKeyPairs !== null) {
            foreach ($this->stringKeyPairs as $key => $value) {
                $key = json_encode($key);
                $pairs[] = "$key => {$value->inspect()}";
            }
        }

        if ($this->intKeyPairs !== null) {
            foreach ($this->intKeyPairs as $key => $value) {
                $pairs[] = "$key => {$value->inspect()}";
            }
        }

        if ($this->true !== null) {
            $pairs[] = "true => {$this->true->inspect()}";
        }

        if ($this->false !== null) {
            $pairs[] = "false => {$this->false->inspect()}";
        }

        return "{" . implode(", ", $pairs) . "}";
    }

    public function lookup(Value $key): ?Value {
        if ($key instanceof StringValue) {
            if ($this->stringKeyPairs !== null) {
                return $this->stringKeyPairs[$key->value] ?? NullValue::$NULL;
            }
        } else if ($key instanceof IntegerValue) {
            if ($this->intKeyPairs !== null) {
                return $this->intKeyPairs[$key->value] ?? NullValue::$NULL;
            }
        } else if ($key instanceof BooleanValue) {
            if ($key->value) {
                return $this->true ?? NullValue::$NULL;
            } else {
                return $this->false ?? NullValue::$NULL;
            }
        }

        return NullValue::$NULL;
    }

    public function set(Value $key, Value $value): bool {
        if ($key instanceof StringValue) {
            if ($this->stringKeyPairs === null) {
                $this->stringKeyPairs = [];
            }
            $this->stringKeyPairs[$key->value] = $value;
            return true;
        } else if ($key instanceof IntegerValue) {
            if ($this->intKeyPairs === null) {
                $this->intKeyPairs = [];
            }
            $this->intKeyPairs[$key->value] = $value;
            return true;
        } else if ($key instanceof BooleanValue) {
            if ($key->value) {
                $this->true = $value;
            } else {
                $this->false = $value;
            }
            return true;
        }
        return false;
    }
}

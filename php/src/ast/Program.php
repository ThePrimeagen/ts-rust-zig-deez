<?php

require 'vendor/autoload.php';

class Program implements Node
{
    /** @var Statement[] */
    public array $statements;

    public function __construct()
    {
        $this->statements = [];
    }


    public function tokenLiteral(): string
    {
        if (count($this->statements) > 0) {
            return $this->statements[0]->tokenLiteral();
        } else {
            return "";
        }
    }

    public function appendStatement(Statement $statement): void
    {
        $this->statements[] = $statement;
    }

    public function __toString(): string
    {
        return implode("", $this->statements);
    }
}
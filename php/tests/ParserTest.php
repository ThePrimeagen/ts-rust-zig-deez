<?php

declare(strict_types=1);

use PHPUnit\Framework\TestCase;

final class ParserTest extends TestCase
{
    public function testParser(): void
    {
        $parser = new Parser(new Tokenizer("10000000000000000000000000 <   0000001 + 5"));
        $program = $parser->parseProgram();
        $this->assertNotEmpty($parser->getErrors());

        $parser = new Parser(new Tokenizer("!true + 3 * (3 + 3) + -----false * !0"));
        $program = $parser->parseProgram();
        $this->assertEquals("(((!true) + (3 * (3 + 3))) + ((-(-(-(-(-false))))) * (!0)));", $program);

        $parser = new Parser(new Tokenizer("if (x < y) {x} else {if (y > x) {y}}"));
        $program = $parser->parseProgram();
        $this->assertEquals("if (x < y) {x;} else {if (y > x) {y;};};", $program);

        $parser = new Parser(new Tokenizer("fn (x, y) {x + y}"));
        $program = $parser->parseProgram();
        $this->assertEquals("fn(x, y) {(x + y);};", $program);
        

        $parser = new Parser(new Tokenizer("add(1, 2+3, 4 + 5)"));
        $program = $parser->parseProgram();
        $this->assertEquals("add(1, (2 + 3), (4 + 5));", $program);
        

        $parser = new Parser(new Tokenizer("add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"));
        $program = $parser->parseProgram();
        $this->assertEquals("add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));", $program);
        

        $parser = new Parser(new Tokenizer("return(true(10));"));
        $program = $parser->parseProgram();
        $this->assertEquals("return true(10);", $program);
        

        $parser = new Parser(new Tokenizer("let f = 10;"));
        $program = $parser->parseProgram();
        $this->assertEquals("let f = 10;", $program);
        

        $parser = new Parser(new Tokenizer("true"));
        $program = $parser->parseProgram();
        $this->assertEquals("true;", $program);
        

        $parser = new Parser(new Tokenizer("true == false"));
        $program = $parser->parseProgram();
        $this->assertEquals("(true == false);", $program);
        

        $parser = new Parser(new Tokenizer("if(true){x}"));
        $program = $parser->parseProgram();
        $this->assertEquals("if true {x;};", $program);
        

        $parser = new Parser(new Tokenizer('"hello\"\t"'));
        $program = $parser->parseProgram();
        $this->assertEquals('"hello\"\t";', $program);
        

        $parser = new Parser(new Tokenizer('"hello I am\"unterminated'));
        $program = $parser->parseProgram();
        $this->assertEquals("", $program);
        $this->assertNotEmpty($parser->getErrors());
        
    }
}

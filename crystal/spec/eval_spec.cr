require "./spec_helper"

describe Evaluator do
  describe BaseValue do
    it "evaluates empty statements" do
      result = eval("")
      result.should be_a NullValue
    end

    it "evaluates let statements" do
      result = eval("let foo = 12;")
      result.should be_a IntegerValue
      result.as(IntegerValue).value.should eq 12
    end

    it "evaluates function expressions" do
      result = eval("fn(x) { return x; };")
      result.should be_a FunctionValue

      func = result.as(FunctionValue)
      func.parameters.size.should eq 1
      func.parameters[0].should eq Identifier.new("x")
      func.body.statements.size.should eq 1
    end
  end

  describe "prefix expressions" do
    it "evaluates signed integers" do
      result = eval("let bar = -42;")
      result.should be_a IntegerValue
      result.as(IntegerValue).value.should eq -42
    end

    it "evaluates inverse booleans" do
      result = eval("let not = !true;")
      result.should be_a BooleanValue
      result.as(BooleanValue).value?.should be_false
    end

    it "evaluates inverse expressions" do
      result = eval("let maybe = !!false;")
      result.should be_a BooleanValue
      result.as(BooleanValue).value?.should be_false
    end
  end

  describe "infix expressions" do
    it "evaluates integer addition" do
      result = eval <<-MONKEY
        let foo = 12;
        let bar = 34;
        let baz = foo + bar;
        MONKEY

      result.should be_a IntegerValue
      result.as(IntegerValue).value.should eq 46
    end

    it "evaluates integer subtraction" do
      result = eval("let minus = 5 - 10;")
      result.should be_a IntegerValue
      result.as(IntegerValue).value.should eq -5
    end

    it "evaluates integer multiplication" do
      result = eval <<-MONKEY
        let word = "deez";
        let double = len(word) * 2;
        MONKEY

      result.should be_a IntegerValue
      result.as(IntegerValue).value.should eq 8
    end

    it "evaluates integer division" do
      result = eval("let zero = 5 / 10;")
      result.should be_a IntegerValue
      result.as(IntegerValue).value.should eq 0
    end

    it "evaluates division by zero" do
      result = eval("let unknown = 0 / 0;")
      result.should be_a ErrorValue
      result.as(ErrorValue).message.should eq "arithmetic overflow"
    end

    it "evaluates string concatenation" do
      result = eval <<-MONKEY
        let hello = "hello";
        let world = "world";
        let message = hello + " " + world;
        MONKEY

      result.should be_a StringValue
      result.as(StringValue).value.should eq "hello world"
    end
  end

  describe "functions" do
    it "evaluates basic functions" do
      result = eval <<-MONKEY
        let add = fn(a, b) {
          return a + b;
        };

        let text = "crystal deez";
        len(text) == add(7, 5);
        MONKEY

      result.should be_a BooleanValue
      result.as(BooleanValue).value?.should be_true
    end

    it "evaluates recursive functions" do
      result = eval <<-MONKEY
        let fib = fn(n) {
          if (n < 2) {
            return 1;
          }

          return fib(n - 2) + fib(n - 1);
        };

        fib(12);
        MONKEY

      result.should be_a IntegerValue
      result.as(IntegerValue).value.should eq 233
    end

    # TODO: maybe add more function examples
  end
end

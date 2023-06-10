require "./spec_helper"

describe Parser do
  describe Expression do
    it "parses empty expressions" do
      statements = parse("")
      statements.should be_empty
    end

    it "parses identifiers" do
      statements = parse("foo_bar")

      expr = statements[0].as(ExpressionStatement).expression
      expr.should be_a Identifier
      expr.as(Identifier).value.should eq "foo_bar"
    end

    it "parses integer literals" do
      statements = parse("12345")

      expr = statements[0].as(ExpressionStatement).expression
      expr.should be_a IntegerLiteral
      expr.as(IntegerLiteral).value.should eq 12345
    end

    pending "parses string literals" do
    end

    it "parses boolean literals" do
      statements = parse("true")

      expr = statements[0].as(ExpressionStatement).expression
      expr.should be_a BooleanLiteral
      expr.as(BooleanLiteral).value?.should be_true

      statements = parse("false")

      expr = statements[0].as(ExpressionStatement).expression
      expr.should be_a BooleanLiteral
      expr.as(BooleanLiteral).value?.should be_false
    end

    pending "parses function literals" do
    end

    pending "parses function calls" do
    end

    pending "parses prefix expressions" do
    end

    pending "parses infix expressions" do
    end

    pending "parses if expressions" do
    end
  end

  describe Statement do
    describe Let do
      it "parses let statements: integers" do
        statements = parse("let foo = 123;")
        statements[0].should be_a Let

        let = statements[0].as(Let)
        let.name.should eq Identifier.new("foo")
        let.value.should be_a ExpressionStatement
        let.value.expression.should be_a IntegerLiteral
        let.value.expression.as(IntegerLiteral).value.should eq 123
      end

      it "parses let statements: strings" do
        statements = parse(%(let foo = "bar";))
        statements[0].should be_a Let

        let = statements[0].as(Let)
        let.name.should eq Identifier.new("foo")
        let.value.should be_a ExpressionStatement
        let.value.expression.should be_a StringLiteral
        let.value.expression.as(StringLiteral).value.should eq "bar"
      end

      it "parses let statements: booleans" do
        statements = parse("let foo = true;")
        statements[0].should be_a Let

        let = statements[0].as(Let)
        let.name.should eq Identifier.new("foo")
        let.value.should be_a ExpressionStatement
        let.value.expression.should be_a BooleanLiteral
        let.value.expression.as(BooleanLiteral).value?.should be_true
      end

      it "parses let statements: function literals" do
        statements = parse("let foo = fn() {};")
        statements[0].should be_a Let

        let = statements[0].as(Let)
        let.name.should eq Identifier.new("foo")
        let.value.should be_a ExpressionStatement
        let.value.expression.should be_a FunctionLiteral

        func = let.value.expression.as(FunctionLiteral)
        func.parameters.should be_empty
        func.body.statements.should be_empty
      end

      it "parses let statements: function calls" do
        statements = parse(%(let foo = len("foo");))
        statements[0].should be_a Let

        let = statements[0].as(Let)
        let.name.should eq Identifier.new("foo")
        let.value.should be_a ExpressionStatement
        let.value.expression.should be_a Call

        call = let.value.expression.as(Call)
        call.function.should be_a Identifier
        call.function.should eq Identifier.new("len")
        call.arguments.size.should eq 1
        call.arguments[0].should be_a StringLiteral
        call.arguments[0].as(StringLiteral).value.should eq "foo"
      end
    end

    describe Return do
      it "parses return statements" do
        statements = parse("return;")
        statements[0].should be_a Return
        statements[0].as(Return).value.should be_nil
      end

      it "parses return values" do
        statements = parse("return true;")
        statements[0].should be_a Return

        ret = statements[0].as(Return)
        ret.value.should be_a BooleanLiteral
        ret.value.as(BooleanLiteral).value?.should be_true
      end
    end

    pending Block do
    end
  end

  describe FunctionLiteral do
    it "parses anonymous functions" do
      statements = parse("fn() {};")

      expr = statements[0].as(ExpressionStatement).expression
      expr.should be_a FunctionLiteral

      expr = expr.as(FunctionLiteral)
      expr.parameters.should be_empty
      expr.body.statements.should be_empty
    end

    it "parses bound functions" do
      statements = parse("let noop = fn() {};")
      statements[0].should be_a Let

      let = statements[0].as(Let)
      let.name.should eq Identifier.new("noop")
      let.value.should be_a ExpressionStatement

      expr = let.value.as(ExpressionStatement).expression
      expr.should be_a FunctionLiteral

      expr = expr.as(FunctionLiteral)
      expr.parameters.should be_empty
      expr.body.statements.should be_empty
    end

    it "parses function return statements" do
      statements = parse("let noop = fn() { return; };")
      statements[0].should be_a Let

      expr = statements[0].as(Let).value.as(ExpressionStatement).expression
      expr.should be_a FunctionLiteral

      expr = expr.as(FunctionLiteral)
      expr.parameters.should be_empty
      expr.body.statements[0].should be_a Return
      expr.body.statements[0].as(Return).value.should be_nil

      statements = parse("let zero = fn() { return 0; };")
      expr = statements[0].as(Let).value.as(ExpressionStatement).expression
      expr.should be_a FunctionLiteral

      expr = expr.as(FunctionLiteral)
      expr.parameters.should be_empty
      expr.body.statements[0].should be_a Return
      expr.body.statements[0].as(Return).value.should be_a IntegerLiteral
      expr.body.statements[0].as(Return).value.as(IntegerLiteral).value.should eq 0
    end

    it "parses function arguments" do
      statements = parse("let foo = fn(x, y) {};")
      statements[0].should be_a Let

      expr = statements[0].as(Let).value.as(ExpressionStatement).expression
      expr.should be_a FunctionLiteral

      expr = expr.as(FunctionLiteral)
      expr.parameters.size.should eq 2
      expr.parameters[0].should eq Identifier.new("x")
      expr.parameters[1].should eq Identifier.new("y")
      expr.body.statements.should be_empty
    end

    it "parses calls in function bodies" do
      statements = parse("let foo = fn(x) { return bar(x); };")
      statements[0].should be_a Let

      expr = statements[0].as(Let).value.as(ExpressionStatement).expression
      expr.should be_a FunctionLiteral

      expr = expr.as(FunctionLiteral)
      expr.parameters[0].should eq Identifier.new("x")
      expr.body.statements[0].should be_a Return
      expr.body.statements[0].as(Return).value.should be_a Call

      call = expr.body.statements[0].as(Return).value.as(Call)
      call.function.should eq Identifier.new("bar")
      call.arguments[0].should eq Identifier.new("x")
    end
  end

  describe Call do
    it "parses no arguments" do
      statements = parse("print();")

      expr = statements[0].as(ExpressionStatement).expression
      expr.should be_a Call

      expr = expr.as(Call)
      expr.function.should eq Identifier.new("print")
      expr.arguments.should be_empty
    end

    it "parses single arguments" do
      statements = parse("print(123);")

      expr = statements[0].as(ExpressionStatement).expression
      expr.should be_a Call

      expr = expr.as(Call)
      expr.function.should eq Identifier.new("print")
      expr.arguments.size.should eq 1
      expr.arguments[0].should be_a IntegerLiteral
      expr.arguments[0].as(IntegerLiteral).value.should eq 123
    end

    it "parses multiple arguments" do
      statements = parse("print(123, 456, 789);")

      expr = statements[0].as(ExpressionStatement).expression
      expr.should be_a Call

      expr = expr.as(Call)
      expr.function.should eq Identifier.new("print")
      expr.arguments.size.should eq 3
      expr.arguments[0].should be_a IntegerLiteral
      expr.arguments[0].as(IntegerLiteral).value.should eq 123
      expr.arguments[1].should be_a IntegerLiteral
      expr.arguments[1].as(IntegerLiteral).value.should eq 456
      expr.arguments[2].should be_a IntegerLiteral
      expr.arguments[2].as(IntegerLiteral).value.should eq 789
    end
  end

  describe Prefix do
    it "parses signed-expressions" do
      statements = parse("-23")

      expr = statements[0].as(ExpressionStatement).expression
      expr.should be_a Prefix

      expr = expr.as(Prefix)
      expr.operator.should eq Prefix::Operator::Negative
      expr.right.should be_a IntegerLiteral
      expr.right.as(IntegerLiteral).value.should eq 23
    end

    it "parses not-expressions" do
      statements = parse("!true")

      expr = statements[0].as(ExpressionStatement).expression
      expr.should be_a Prefix

      expr = expr.as(Prefix)
      expr.operator.should eq Prefix::Operator::Not
      expr.right.should be_a BooleanLiteral
      expr.right.as(BooleanLiteral).value?.should be_true
    end
  end

  describe Infix do
    {% for op, type in {
                         "+" => "Add",
                         "-" => "Subtract",
                         "*" => "Multiply",
                         "/" => "Divide",
                       } %}
      it "parses {{ type.downcase.id }} operators" do
        statements = parse("6 {{ op.id }} 4;")

        expr = statements[0].as(ExpressionStatement).expression
        expr.should be_a Infix

        expr = expr.as(Infix)
        expr.left.should be_a IntegerLiteral
        expr.left.as(IntegerLiteral).value.should eq 6
        expr.operator.should eq Infix::Operator::{{ type.id }}
        expr.right.should be_a IntegerLiteral
        expr.right.as(IntegerLiteral).value.should eq 4
      end
    {% end %}
  end
end

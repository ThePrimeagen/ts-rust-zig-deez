require "./spec_helper"

describe Parser do
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

  it "parses boolean-true literals" do
    statements = parse("true")

    expr = statements[0].as(ExpressionStatement).expression
    expr.should be_a BooleanLiteral
    expr.as(BooleanLiteral).value.should be_true
  end

  it "parses boolean-false literals" do
    statements = parse("false")

    expr = statements[0].as(ExpressionStatement).expression
    expr.should be_a BooleanLiteral
    expr.as(BooleanLiteral).value.should be_false
  end

  it "parses let statements" do
    statements = parse("let foo = 123;")

    statements[0].should be_a Let

    let = statements[0].as(Let)
    let.name.should be_a Identifier
    let.value.should be_a ExpressionStatement
    let.value.expression.should be_a IntegerLiteral
    let.value.expression.as(IntegerLiteral).value.should eq 123
  end

  describe "function calls" do
    it "parses no arguments" do
      statements = parse("print();")

      expr = statements[0].as(ExpressionStatement).expression
      expr.should be_a Call
      expr = expr.as(Call)

      expr.function.should be_a Identifier
      expr.arguments.should be_empty
    end

    it "parses single arguments" do
      statements = parse("print(123);")

      expr = statements[0].as(ExpressionStatement).expression
      expr.should be_a Call
      expr = expr.as(Call)

      expr.function.should be_a Identifier
      expr.arguments.size.should eq 1
      expr.arguments[0].should be_a IntegerLiteral
      expr.arguments[0].as(IntegerLiteral).value.should eq 123
    end

    it "parses multiple arguments" do
      statements = parse("print(123, 456, 789);")

      expr = statements[0].as(ExpressionStatement).expression
      expr.should be_a Call
      expr = expr.as(Call)

      expr.function.should be_a Identifier
      expr.arguments.size.should eq 3
      expr.arguments[0].should be_a IntegerLiteral
      expr.arguments[0].as(IntegerLiteral).value.should eq 123
      expr.arguments[1].should be_a IntegerLiteral
      expr.arguments[1].as(IntegerLiteral).value.should eq 456
      expr.arguments[2].should be_a IntegerLiteral
      expr.arguments[2].as(IntegerLiteral).value.should eq 789
    end
  end
end

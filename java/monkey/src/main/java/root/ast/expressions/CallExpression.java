package root.ast.expressions;

import root.LocalizedToken;
import root.ast.Node;

import java.util.List;
import java.util.stream.Collectors;

public class CallExpression extends Expression {

    private final Expression function;
    private final List<Expression> arguments;

    public CallExpression(LocalizedToken token, Expression function, List<Expression> arguments) {
        super(token);
        this.function = function;
        this.arguments = arguments;
    }

    public Expression getFunction() {
        return function;
    }

    public List<Expression> getArguments() {
        return arguments;
    }

    @Override
    public String stringRep() {
        var arguments = this.arguments.stream().map(Node::stringRep).collect(Collectors.joining(", "));
        return "%s(%s)".formatted(function.stringRep(), arguments);
    }
}

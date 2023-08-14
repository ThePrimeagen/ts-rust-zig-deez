package root.ast.expressions;

import root.LocalizedToken;
import root.Token;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class CallExpression extends Expression {

    private final Expression function;
    private final List<Expression> arguments = new ArrayList<>();

    public CallExpression(LocalizedToken token, Expression function) {
        this.function = function;
        this.token = token;
    }

    public Expression getFunction() {
        return function;
    }

    public List<Expression> getArguments() {
        return arguments;
    }

    @Override
    public String toString() {
        var arguments = this.arguments.stream().map(Object::toString).collect(Collectors.joining(", "));
        return "%s(%s)".formatted(function, arguments);
    }
}

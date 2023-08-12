package root.ast.expressions;

import root.Token;
import root.ast.statements.BlockStatement;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class FunctionLiteralExpression extends Expression {

    private final List<IdentifierExpression> parameters = new ArrayList<>();
    private BlockStatement body;

    public FunctionLiteralExpression(Token token) {
        this.token = token;
    }

    public List<IdentifierExpression> getParameters() {
        return parameters;
    }

    public BlockStatement getBody() {
        return body;
    }

    public void setBody(BlockStatement body) {
        this.body = body;
    }

    @Override
    public String toString() {
        var params = parameters.stream().map(Object::toString).collect(Collectors.joining(", "));
        return "fn(%s) %s".formatted(params, body);
    }
}

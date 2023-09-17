package root.ast.expressions;

import root.LocalizedToken;
import root.ast.Node;
import root.ast.statements.BlockStatement;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class FunctionLiteralExpression extends Expression {

    private final List<IdentifierExpression> parameters = new ArrayList<>();
    private BlockStatement body;

    public FunctionLiteralExpression(LocalizedToken token) {
        super(token);
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
    public String stringRep() {
        var params = parameters.stream().map(Node::stringRep).collect(Collectors.joining(", "));
        return "fn(%s) %s".formatted(params, body.stringRep());
    }
}

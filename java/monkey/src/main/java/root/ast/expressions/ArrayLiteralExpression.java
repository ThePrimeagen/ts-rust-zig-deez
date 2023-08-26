package root.ast.expressions;

import root.LocalizedToken;
import root.ast.Node;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public class ArrayLiteralExpression extends Expression {

    private final List<Expression> elements;

    public ArrayLiteralExpression(LocalizedToken token, List<Expression> elements) {
        this.token = token;
        this.elements = Collections.unmodifiableList(elements);
    }

    public List<Expression> getElements() {
        return elements;
    }

    @Override
    public String toString() {
        return "[%s]".formatted(elements.stream().map(Node::toString).collect(Collectors.joining(", ")));
    }
}

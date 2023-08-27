package root.ast.expressions;

import root.LocalizedToken;
import root.ast.Node;

public abstract class Expression extends Node {
    public Expression(LocalizedToken token) {
        super(token);
    }
}

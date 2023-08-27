package root.ast.statements;

import root.LocalizedToken;
import root.ast.Node;

public abstract class Statement extends Node {
    public Statement(LocalizedToken token) {
        super(token);
    }
}


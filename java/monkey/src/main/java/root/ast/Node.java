package root.ast;

import root.LocalizedToken;

public abstract class Node {

    private final LocalizedToken token;

    public Node(LocalizedToken token) {
        this.token = token;
    }

    public LocalizedToken getToken() {
        return token;
    }

    public String tokenLiteral() {
        return token.token().literal();
    }

    abstract public String stringRep();

    @Override
    public String toString() {
        return getClass().getSimpleName() + " " + stringRep();
    }
}

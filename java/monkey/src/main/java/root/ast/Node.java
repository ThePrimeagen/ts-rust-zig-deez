package root.ast;

import root.LocalizedToken;
import root.Token;

public abstract class Node {

    protected LocalizedToken token;

    public LocalizedToken getToken() {
        return token;
    }

    public void setToken(LocalizedToken token) {
        this.token = token;
    }

    public String tokenLiteral() {
        return token.token().literal();
    }
}

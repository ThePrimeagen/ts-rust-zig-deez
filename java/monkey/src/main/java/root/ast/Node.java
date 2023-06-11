package root.ast;

import root.Token;

public abstract class Node {

    protected Token token;

    public Token getToken() {
        return token;
    }

    public void setToken(Token token) {
        this.token = token;
    }

    public String tokenLiteral() {
        return token.literal();
    }
}

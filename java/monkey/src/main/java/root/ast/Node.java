package root.ast;

import root.Token;

public abstract class Node {

    public Token token;

    public String tokenLiteral() {
        return token.literal();
    }
}

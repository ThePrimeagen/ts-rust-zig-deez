package root.ast;

import root.Token;

public class ReturnStatement extends Statement {

    public Token token;
    public Expression returnValue;

    public ReturnStatement(Token token) {
        this.token = token;
    }
}

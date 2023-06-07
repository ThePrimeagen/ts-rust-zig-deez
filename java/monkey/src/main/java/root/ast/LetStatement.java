package root.ast;

import root.Token;

public class LetStatement extends Statement {

    public Token token;
    public Identifier name;
    public Expression value;

    public LetStatement(Token token) {
        this.token = token;
    }

    @Override
    public String tokenLiteral() {
        return token.literal();
    }

    @Override
    public void statementNode() {
    }
}

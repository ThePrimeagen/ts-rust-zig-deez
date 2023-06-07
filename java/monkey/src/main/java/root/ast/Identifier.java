package root.ast;

import root.Token;

public class Identifier extends Expression {

    public String value;

    public Identifier(Token token, String value) {
        this.token = token;
        this.value = value;
    }

    @Override
    public String toString() {
        return value;
    }
}

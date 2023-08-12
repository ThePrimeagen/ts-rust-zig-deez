package root.ast.statements;

import root.Token;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class BlockStatement extends Statement {

    private final List<Statement> statements = new ArrayList<>();

    public BlockStatement(Token token) {
        this.token = token;
    }

    public List<Statement> getStatements() {
        return statements;
    }

    public void addStatement(Statement statement) {
        statements.add(statement);
    }

    @Override
    public String toString() {
        var statements = this.statements.stream().map(Object::toString).collect(Collectors.joining("\n"));
        return "{\n%s\n}".formatted(statements);
    }
}

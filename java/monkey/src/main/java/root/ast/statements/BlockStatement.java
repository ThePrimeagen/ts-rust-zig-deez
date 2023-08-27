package root.ast.statements;

import root.LocalizedToken;
import root.ast.Node;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class BlockStatement extends Statement {

    private final List<Statement> statements = new ArrayList<>();

    public BlockStatement(LocalizedToken token) {
        super(token);
    }

    public List<Statement> getStatements() {
        return statements;
    }

    public void addStatement(Statement statement) {
        statements.add(statement);
    }

    @Override
    public String stringRep() {
        var statements = this.statements.stream().map(Node::stringRep).collect(Collectors.joining("\n"));
        return "{\n%s\n}".formatted(statements);
    }
}

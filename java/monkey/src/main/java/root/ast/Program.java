package root.ast;

import root.ast.statements.Statement;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Program extends Node {

    private final List<Statement> statements = new ArrayList<>();

    public Program() {
        super(null);
    }

    public List<Statement> getStatements() {
        return statements;
    }

    @Override
    public String tokenLiteral() {
        if (statements.isEmpty()) {
            return "";
        }
        return statements.get(0).tokenLiteral();
    }

    @Override
    public String stringRep() {
        return statements.stream().map(Node::stringRep).collect(Collectors.joining("\n"));
    }
}

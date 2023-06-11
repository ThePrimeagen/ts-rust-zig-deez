package root.ast;

import root.ast.statements.Statement;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Program extends Node {

    public final List<Statement> statements = new ArrayList<>();

    @Override
    public String tokenLiteral() {
        if (statements.isEmpty()) {
            return "";
        }
        return statements.get(0).tokenLiteral();
    }

    @Override
    public String toString() {
        return statements.stream().map(Object::toString).collect(Collectors.joining("\n"));
    }
}

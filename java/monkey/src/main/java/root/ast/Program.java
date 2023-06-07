package root.ast;

import java.util.ArrayList;
import java.util.List;

public class Program {

    public final List<Statement> statements = new ArrayList<>();

    public String TokenLiteral() {
        if (statements.isEmpty()) {
            return "";
        }
        return statements.get(0).tokenLiteral();
    }
}

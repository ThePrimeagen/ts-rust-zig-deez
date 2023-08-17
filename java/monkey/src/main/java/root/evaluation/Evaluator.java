package root.evaluation;

import root.ast.Node;
import root.ast.Program;
import root.ast.expressions.BooleanLiteralExpression;
import root.ast.expressions.IntegerLiteralExpression;
import root.ast.statements.ExpressionStatement;
import root.ast.statements.Statement;
import root.evaluation.objects.MonkeyObject;
import root.evaluation.objects.impl.MonkeyBoolean;
import root.evaluation.objects.impl.MonkeyInteger;
import root.evaluation.objects.impl.MonkeyNull;

import java.util.List;

public class Evaluator {

    public static MonkeyObject<?> eval(Node node) {
        return switch (node) {
            case Program program -> evalStatements(program.getStatements());
            case ExpressionStatement expression -> eval(expression.getExpression());
            case IntegerLiteralExpression integerLiteral -> new MonkeyInteger(integerLiteral.getValue());
            case BooleanLiteralExpression booleanLiteral ->
                    booleanLiteral.getValue() ? MonkeyBoolean.TRUE : MonkeyBoolean.FALSE;
            default -> throw new IllegalStateException("Unexpected value: " + node);
        };
    }

    private static MonkeyObject<?> evalStatements(List<Statement> statements) {
        MonkeyObject<?> object = MonkeyNull.INSTANCE;

        for (var statement : statements) {
            object = eval(statement);
        }

        return object;
    }
}

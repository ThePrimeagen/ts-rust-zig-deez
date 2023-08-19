package root.evaluation;

import root.TokenType;
import root.ast.Node;
import root.ast.Program;
import root.ast.expressions.BooleanLiteralExpression;
import root.ast.expressions.IntegerLiteralExpression;
import root.ast.expressions.PrefixExpression;
import root.ast.statements.ExpressionStatement;
import root.ast.statements.Statement;
import root.evaluation.objects.MonkeyObject;
import root.evaluation.objects.impl.MonkeyBoolean;
import root.evaluation.objects.impl.MonkeyInteger;
import root.evaluation.objects.impl.MonkeyNull;

import java.util.List;
import java.util.Objects;

public class Evaluator {

    public static MonkeyObject<?> eval(Node node) {
        return switch (node) {
            case Program program -> evalStatements(program.getStatements());

            case ExpressionStatement expression -> eval(expression.getExpression());

            case IntegerLiteralExpression integerLiteral -> new MonkeyInteger(integerLiteral.getValue());

            case BooleanLiteralExpression booleanLiteral ->
                    booleanLiteral.getValue() ? MonkeyBoolean.TRUE : MonkeyBoolean.FALSE;

            case PrefixExpression prefixExpression -> parsePrefixExpression(prefixExpression);

            default -> throw new IllegalStateException("Unexpected value: %s %s".formatted(node.getClass().getSimpleName(), node));
        };
    }

    private static MonkeyObject<?> evalStatements(List<Statement> statements) {
        MonkeyObject<?> object = MonkeyNull.INSTANCE;

        for (var statement : statements) {
            object = eval(statement);
        }

        return object;
    }

    private static MonkeyObject<?> parsePrefixExpression(PrefixExpression prefixExpression) {
        MonkeyObject<?> expressionResult = eval(prefixExpression.getRight());

        return switch (prefixExpression.getToken().type()) {
            case BANG -> switch (expressionResult) {
                case MonkeyBoolean aBoolean -> aBoolean.getValue() ? MonkeyBoolean.FALSE : MonkeyBoolean.TRUE;
                case MonkeyNull ignored -> MonkeyBoolean.TRUE;
                default -> MonkeyBoolean.FALSE;
            };

            case MINUS -> {
                if (expressionResult instanceof MonkeyInteger integer) {
                    yield new MonkeyInteger(-integer.getValue());
                }

                yield MonkeyNull.INSTANCE;
            }

            default -> MonkeyNull.INSTANCE;
        };
    }
}

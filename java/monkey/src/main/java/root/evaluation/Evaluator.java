package root.evaluation;

import root.ast.Node;
import root.ast.Program;
import root.ast.expressions.BooleanLiteralExpression;
import root.ast.expressions.InfixExpression;
import root.ast.expressions.IntegerLiteralExpression;
import root.ast.expressions.PrefixExpression;
import root.ast.statements.ExpressionStatement;
import root.ast.statements.Statement;
import root.evaluation.objects.MonkeyObject;
import root.evaluation.objects.ObjectType;
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
            case BooleanLiteralExpression booleanLiteral -> MonkeyBoolean.nativeToMonkey(booleanLiteral.getValue());
            case PrefixExpression prefixExpression -> evalPrefixExpression(prefixExpression);
            case InfixExpression infixExpression -> evalInfixExpression(infixExpression);
            default ->
                    throw new IllegalStateException("Unexpected value: %s %s".formatted(node.getClass().getSimpleName(), node));
        };
    }

    private static MonkeyObject<?> evalStatements(List<Statement> statements) {
        MonkeyObject<?> object = MonkeyNull.INSTANCE;

        for (var statement : statements) {
            object = eval(statement);
        }

        return object;
    }

    private static MonkeyObject<?> evalPrefixExpression(PrefixExpression prefixExpression) {
        MonkeyObject<?> expressionResult = eval(prefixExpression.getRight());

        return switch (prefixExpression.getToken().type()) {
            case BANG -> switch (expressionResult) {
                case MonkeyBoolean aBoolean -> MonkeyBoolean.nativeToMonkey(!aBoolean.getValue());
                case MonkeyNull ignored -> MonkeyBoolean.TRUE;
                default -> MonkeyBoolean.FALSE;
            };

            case MINUS -> {
                if (expressionResult instanceof MonkeyInteger integer) {
                    yield new MonkeyInteger(-integer.getValue());
                }

                yield MonkeyNull.INSTANCE;
            }
            default -> throw new IllegalStateException("Unexpected value: " + prefixExpression.getToken().type());
        };
    }

    private static MonkeyObject<?> evalInfixExpression(InfixExpression infixExpression) {
        MonkeyObject<?> left = eval(infixExpression.getLeft());
        MonkeyObject<?> rigth = eval(infixExpression.getRight());

        if (left instanceof MonkeyInteger integerLeft && rigth instanceof MonkeyInteger integerRight) {
            return evalIntegerInfixExpression(infixExpression, integerLeft, integerRight);
        }

        return switch (infixExpression.getToken().type()) {
            case EQUAL -> MonkeyBoolean.nativeToMonkey(left == rigth);
            case NOT_EQUAL -> MonkeyBoolean.nativeToMonkey(left != rigth);

            default -> throw new IllegalStateException("Unexpected value: " + infixExpression.getToken().type());
        };
    }

    private static MonkeyObject<?> evalIntegerInfixExpression(InfixExpression infixExpression, MonkeyInteger left, MonkeyInteger right) {
        return switch (infixExpression.getToken().type()) {
            case PLUS -> new MonkeyInteger(left.getValue() + right.getValue());
            case MINUS -> new MonkeyInteger(left.getValue() - right.getValue());
            case ASTERISK -> new MonkeyInteger(left.getValue() * right.getValue());
            case SLASH -> new MonkeyInteger(left.getValue() / right.getValue());

            case EQUAL -> MonkeyBoolean.nativeToMonkey(left.getValue() == right.getValue());
            case NOT_EQUAL -> MonkeyBoolean.nativeToMonkey(left.getValue() != right.getValue());
            case LT -> MonkeyBoolean.nativeToMonkey(left.getValue() < right.getValue());
            case GT -> MonkeyBoolean.nativeToMonkey(left.getValue() > right.getValue());

            default -> throw new IllegalStateException("Unexpected value: " + infixExpression.getToken().type());
        };
    }
}

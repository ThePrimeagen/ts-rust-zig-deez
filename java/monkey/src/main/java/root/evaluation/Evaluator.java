package root.evaluation;

import root.ast.Node;
import root.ast.Program;
import root.ast.expressions.*;
import root.ast.statements.BlockStatement;
import root.ast.statements.ExpressionStatement;
import root.ast.statements.ReturnStatement;
import root.ast.statements.Statement;
import root.evaluation.objects.MonkeyObject;
import root.evaluation.objects.impl.*;

import java.util.List;

public class Evaluator {

    public static MonkeyObject<?> eval(Node node) throws EvaluationException {
        return switch (node) {
            case Program program -> evalStatements(program.getStatements(), true);
            case BlockStatement blockStatement -> evalStatements(blockStatement.getStatements(), false);
            case ExpressionStatement expression -> eval(expression.getExpression());
            case IntegerLiteralExpression integerLiteral -> new MonkeyInteger(integerLiteral.getValue());
            case BooleanLiteralExpression booleanLiteral -> MonkeyBoolean.nativeToMonkey(booleanLiteral.getValue());
            case PrefixExpression prefixExpression -> evalPrefixExpression(prefixExpression);
            case InfixExpression infixExpression -> evalInfixExpression(infixExpression);
            case IfExpression ifExpression -> evalIfExpression(ifExpression);
            case ReturnStatement returnStatement -> {
                MonkeyObject<?> returnValue = eval(returnStatement.getReturnValue());
                yield new MonkeyReturn<>(returnValue);
            }

            case UnitExpression ignored -> MonkeyUnit.INSTANCE;
            case NullLiteralExpression ignored -> MonkeyNull.INSTANCE;

            // Should be impossible (after everything is implemented)
            default ->
                    throw new IllegalStateException("Unexpected value (unreachable code): %s %s".formatted(
                            node.getClass().getSimpleName(),
                            node
                    ));
        };
    }

    private static MonkeyObject<?> evalStatements(List<Statement> statements, boolean unwrapReturn) throws EvaluationException {
        MonkeyObject<?> object = MonkeyNull.INSTANCE;

        for (var statement : statements) {
            object = eval(statement);
            // Maybe an exception is more idiomatic? What are the performance implications of this choice?
            if (object instanceof MonkeyReturn<?> monkeyReturn) {
                if (unwrapReturn) {
                    return monkeyReturn.returnValue;
                }
                return monkeyReturn;
            }
        }

        return object;
    }

    private static MonkeyObject<?> evalPrefixExpression(PrefixExpression prefixExpression) throws EvaluationException {
        MonkeyObject<?> expressionResult = eval(prefixExpression.getRight());

        return switch (prefixExpression.getToken().type()) {
            case BANG -> MonkeyBoolean.nativeToMonkey(!isTruthy(expressionResult));

            case MINUS -> {
                if (expressionResult instanceof MonkeyInteger integer) {
                    yield new MonkeyInteger(-integer.getValue());
                }

                if (expressionResult instanceof MonkeyNull) {
                    yield MonkeyNull.INSTANCE;
                }

                throw new EvaluationException(prefixExpression.getToken(), "Operation - not supported for type %s", expressionResult.getType());
            }

            // Should be impossible
            default ->
                    throw new IllegalStateException("Unexpected value (unreachable code): " + prefixExpression.getToken().type());
        };
    }

    private static MonkeyObject<?> evalInfixExpression(InfixExpression infixExpression) throws EvaluationException {
        MonkeyObject<?> left = eval(infixExpression.getLeft());
        MonkeyObject<?> rigth = eval(infixExpression.getRight());

        if (left instanceof MonkeyInteger integerLeft && rigth instanceof MonkeyInteger integerRight) {
            return evalIntegerInfixExpression(infixExpression, integerLeft, integerRight);
        }

        return switch (infixExpression.getToken().type()) {
            case EQUAL -> MonkeyBoolean.nativeToMonkey(left == rigth);
            case NOT_EQUAL -> MonkeyBoolean.nativeToMonkey(left != rigth);

            default -> {
                // ¯\_(ツ)_/¯
                if (left instanceof MonkeyNull || rigth instanceof MonkeyNull) {
                    yield MonkeyNull.INSTANCE;
                }

                throw new EvaluationException(
                        infixExpression.getToken(),
                        "Operation %s not supported for types %s and %s",
                        infixExpression.getToken().literal(),
                        left.getType(),
                        rigth.getType()
                );
            }
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

            // Should be impossible
            default ->
                    throw new IllegalStateException("Unexpected value (unreachable code):" + infixExpression.getToken().type());
        };
    }

    private static MonkeyObject<?> evalIfExpression(IfExpression ifExpression) throws EvaluationException {
        MonkeyObject<?> conditionResult = eval(ifExpression.getCondition());

        if (isTruthy(conditionResult)) {
            return eval(ifExpression.getConsequence());
        } else if (ifExpression.getAlternative() != null) {
            return eval(ifExpression.getAlternative());
        }

        return MonkeyNull.INSTANCE;
    }

    private static boolean isTruthy(MonkeyObject<?> object) {
        return switch (object) {
            case MonkeyBoolean bool -> bool.getValue();
            case MonkeyNull ignored -> false;
            default -> true;
        };
    }
}

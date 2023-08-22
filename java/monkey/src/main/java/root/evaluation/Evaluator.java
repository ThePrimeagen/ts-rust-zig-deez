package root.evaluation;

import root.ast.Node;
import root.ast.Program;
import root.ast.expressions.*;
import root.ast.statements.*;
import root.evaluation.objects.AbstractMonkeyFunction;
import root.evaluation.objects.MonkeyObject;
import root.evaluation.objects.impl.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class Evaluator {

    private final Environment environment;

    public Evaluator() {
        environment = new Environment();
    }

    public Evaluator(Environment environment) {
        this.environment = environment;
    }

    public MonkeyObject<?> eval(Node node) throws EvaluationException {
        return switch (node) {
            case Program program -> evalStatements(program.getStatements(), true);
            case BlockStatement blockStatement -> evalStatements(blockStatement.getStatements(), false);
            case ExpressionStatement expression -> eval(expression.getExpression());
            case IntegerLiteralExpression integerLiteral -> new MonkeyInteger(integerLiteral.getValue());
            case BooleanLiteralExpression booleanLiteral -> MonkeyBoolean.nativeToMonkey(booleanLiteral.getValue());
            case PrefixExpression prefixExpression -> evalPrefixExpression(prefixExpression);
            case InfixExpression infixExpression -> evalInfixExpression(infixExpression);
            case IfExpression ifExpression -> evalIfExpression(ifExpression);
            case ReturnStatement returnStatement -> evalReturnStatement(returnStatement);
            case LetStatement letStatement -> evalLetStatement(letStatement);
            case IdentifierExpression identifier -> evalIdentifierExpression(identifier);
            case FunctionLiteralExpression functionLiteral -> new MonkeyFunction(environment, functionLiteral);
            case CallExpression callExpression -> evalCallExpression(callExpression);
            case UnitExpression ignored -> MonkeyUnit.INSTANCE;
            case NullLiteralExpression ignored -> MonkeyNull.INSTANCE;
            // Should be impossible (after everything is implemented)
            default -> throw new IllegalStateException("Unexpected value (unreachable code): %s %s".formatted(
                    node.getClass().getSimpleName(),
                    node
            ));
        };
    }

    private MonkeyObject<?> evalCallExpression(CallExpression callExpression) throws EvaluationException {
        MonkeyObject<?> objectToCall = eval(callExpression.getFunction());

        if (objectToCall instanceof AbstractMonkeyFunction functionToCall) {
            var arguments = new ArrayList<MonkeyObject<?>>();

            for (var expression : callExpression.getArguments()) {
                arguments.add(eval(expression));
            }

            return unwrapReturnValue(functionToCall.getValue().apply(callExpression.getToken(), arguments));
        } else {
            throw new EvaluationException(callExpression.getToken(), "Cannot call non Function object");
        }
    }

    private MonkeyObject<?> evalStatements(List<Statement> statements, boolean unwrapReturn) throws EvaluationException {
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

    private MonkeyObject<?> evalPrefixExpression(PrefixExpression prefixExpression) throws EvaluationException {
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

    private MonkeyObject<?> evalInfixExpression(InfixExpression infixExpression) throws EvaluationException {
        MonkeyObject<?> left = eval(infixExpression.getLeft());
        MonkeyObject<?> right = eval(infixExpression.getRight());

        if (left instanceof MonkeyInteger integerLeft && right instanceof MonkeyInteger integerRight) {
            return evalIntegerInfixExpression(infixExpression, integerLeft, integerRight);
        }

        return switch (infixExpression.getToken().type()) {
            case EQUAL -> MonkeyBoolean.nativeToMonkey(left == right);
            case NOT_EQUAL -> MonkeyBoolean.nativeToMonkey(left != right);

            default -> {
                // ¯\_(ツ)_/¯
                if (left == MonkeyNull.INSTANCE || right == MonkeyNull.INSTANCE) {
                    yield switch (infixExpression.getToken().type()) {
                        case PLUS, MINUS, ASTERISK, SLASH -> MonkeyNull.INSTANCE;
                        default -> {
                            var detail = left == right ? "both values are" : left == MonkeyNull.INSTANCE ? "left value is" : "right value is";
                            throw new EvaluationException(infixExpression.getToken(), "Null value error: %s null", detail);
                        }
                    };
                }

                throw new EvaluationException(
                        infixExpression.getToken(),
                        "Operation %s not supported for types %s and %s",
                        infixExpression.getToken().literal(),
                        left.getType(),
                        right.getType()
                );
            }
        };
    }

    private MonkeyObject<?> evalIntegerInfixExpression(
            InfixExpression infixExpression,
            MonkeyInteger left,
            MonkeyInteger right
    ) throws EvaluationException {
        return switch (infixExpression.getToken().type()) {
            case PLUS -> new MonkeyInteger(left.getValue() + right.getValue());
            case MINUS -> new MonkeyInteger(left.getValue() - right.getValue());
            case ASTERISK -> new MonkeyInteger(left.getValue() * right.getValue());
            case SLASH -> {
                if (right.getValue() == 0) {
                    throw new EvaluationException(infixExpression.getToken(), "Cannot divide by 0");
                }
                yield new MonkeyInteger(left.getValue() / right.getValue());
            }

            case EQUAL -> MonkeyBoolean.nativeToMonkey(left.getValue() == right.getValue());
            case NOT_EQUAL -> MonkeyBoolean.nativeToMonkey(left.getValue() != right.getValue());
            case LT -> MonkeyBoolean.nativeToMonkey(left.getValue() < right.getValue());
            case GT -> MonkeyBoolean.nativeToMonkey(left.getValue() > right.getValue());

            // Should be impossible
            default ->
                    throw new IllegalStateException("Unexpected value (unreachable code):" + infixExpression.getToken().type());
        };
    }

    private MonkeyObject<?> evalIfExpression(IfExpression ifExpression) throws EvaluationException {
        MonkeyObject<?> conditionResult = eval(ifExpression.getCondition());

        if (isTruthy(conditionResult)) {
            return eval(ifExpression.getConsequence());
        } else if (ifExpression.getAlternative() != null) {
            return eval(ifExpression.getAlternative());
        }

        return MonkeyNull.INSTANCE;
    }

    private MonkeyObject<?> evalIdentifierExpression(IdentifierExpression identifier) throws EvaluationException {
        Optional<MonkeyObject<?>> value = environment.get(identifier.getValue());

        if (value.isEmpty()) {
            return BuiltinFunctions.getFunction(identifier.getValue()).orElseThrow(() ->
                    new EvaluationException(identifier.getToken(), "Variable %s is not declared", identifier.getValue())
            );
        }

        return value.get();
    }

    private MonkeyObject<?> evalLetStatement(LetStatement letStatement) throws EvaluationException {
        MonkeyObject<?> value = eval(letStatement.getValue());
        if (value == MonkeyUnit.INSTANCE) {
            throw new EvaluationException(letStatement.getToken(), "Cannot bind unit (void) to a variable");
        }
        return environment.set(letStatement.getName().getValue(), value);
    }

    private MonkeyObject<?> evalReturnStatement(ReturnStatement returnStatement) throws EvaluationException {
        MonkeyObject<?> returnValue = eval(returnStatement.getReturnValue());
        return new MonkeyReturn<>(returnValue);
    }

    private static boolean isTruthy(MonkeyObject<?> object) {
        return switch (object) {
            case MonkeyBoolean bool -> bool.getValue();
            case MonkeyNull ignored -> false;
            default -> true;
        };
    }

    private static <T> MonkeyObject<T> unwrapReturnValue(MonkeyObject<T> returnValue) {
        if (returnValue instanceof MonkeyReturn<T> monkeyReturn) {
            return monkeyReturn.returnValue;
        }

        return returnValue;
    }
}

package root.evaluation;

import root.TokenType;
import root.ast.Node;
import root.ast.Program;
import root.ast.expressions.*;
import root.ast.statements.*;
import root.evaluation.objects.AbstractMonkeyFunction;
import root.evaluation.objects.HashKey;
import root.evaluation.objects.MonkeyHashable;
import root.evaluation.objects.MonkeyObject;
import root.evaluation.objects.impl.*;

import java.util.*;

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
            case NullLiteralExpression ignored -> MonkeyNull.INSTANCE;
            case StringLiteralExpression string -> new MonkeyString(string.tokenLiteral());
            case ArrayLiteralExpression array -> evalArrayLiteralExpression(array);
            case IndexExpression index -> evalIndexExpression(index);
            case HashLiteralExpression hash -> evalHashLiteralExpression(hash);

            // Should be impossible
            default ->
                    throw new IllegalStateException("INTERPRETER BUG: Unexpected value (unreachable code): %s %s".formatted(
                            node.getClass().getSimpleName(),
                            node
                    ));
        };
    }

    private MonkeyObject<?> evalIndexExpression(IndexExpression index) throws EvaluationException {
        MonkeyObject<?> left = eval(index.getLeft());

        return switch (left) {
            case MonkeyArray array -> {
                MonkeyObject<?> indexer = eval(index.getIndex());

                if (indexer instanceof MonkeyInteger integer) {
                    if (integer.getValue() < 0 || integer.getValue() >= array.getValue().size()) {
                        yield MonkeyNull.INSTANCE;
                    }

                    yield array.getValue().get(integer.getValue().intValue());
                }

                throw new EvaluationException(index.getIndex().getToken(), "Index to an array must be an Expression that yields an Int");
            }
            case MonkeyHash hash -> {
                MonkeyObject<?> indexer = eval(index.getIndex());


                MonkeyHashable hashable = MonkeyHashable.checkIsHashable(indexer, index.getIndex().getToken());
                MonkeyObject<?> objectInMap = hash.getValue().get(hashable.hashKey());

                yield objectInMap == null ? MonkeyNull.INSTANCE : objectInMap;
            }
            default ->
                    throw new EvaluationException(index.getLeft().getToken(), "Index operator not supported for %s", left.getType());
        };
    }

    private MonkeyObject<?> evalHashLiteralExpression(HashLiteralExpression hash) throws EvaluationException {
        var pairs = new LinkedHashMap<HashKey, MonkeyObject<?>>();

        for (HashLiteralExpression.KeyValuePair(Expression key, Expression value) : hash.getPairs()) {
            MonkeyObject<?> keyObject = eval(key);

            if (keyObject instanceof MonkeyHashable hashable) {
                MonkeyObject<?> valueObject = eval(value);
                pairs.put(hashable.hashKey(), valueObject);
            }
        }

        return new MonkeyHash(pairs);
    }

    private MonkeyObject<?> evalArrayLiteralExpression(ArrayLiteralExpression array) throws EvaluationException {
        var elements = evalExpressions(array.getElements());
        return new MonkeyArray(elements);
    }

    private MonkeyObject<?> evalCallExpression(CallExpression callExpression) throws EvaluationException {
        MonkeyObject<?> objectToCall = eval(callExpression.getFunction());

        if (objectToCall instanceof AbstractMonkeyFunction functionToCall) {
            var arguments = evalExpressions(callExpression.getArguments());
            return unwrapReturnValue(functionToCall.getValue().apply(callExpression.getToken(), arguments));
        } else {
            throw new EvaluationException(callExpression.getToken(), "Cannot call non Function object");
        }
    }

    private List<MonkeyObject<?>> evalExpressions(List<Expression> expressions) throws EvaluationException {
        var objects = new ArrayList<MonkeyObject<?>>();

        for (var expression : expressions) {
            objects.add(eval(expression));
        }

        return objects;
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
                    throw new IllegalStateException("INTERPRETER BUG: Unexpected value (unreachable code): " + prefixExpression.getToken().type());
        };
    }

    private MonkeyObject<?> evalInfixExpression(InfixExpression infixExpression) throws EvaluationException {
        MonkeyObject<?> left = eval(infixExpression.getLeft());

        TokenType operation = infixExpression.getToken().type();

        if (operation == TokenType.AND || operation == TokenType.OR) {
            return evalBooleanLogicOperation(infixExpression, left);
        }

        MonkeyObject<?> right = eval(infixExpression.getRight());

        if (left instanceof MonkeyInteger integerLeft && right instanceof MonkeyInteger integerRight) {
            return evalIntegerInfixExpression(infixExpression, integerLeft, integerRight);
        }

        if (left instanceof MonkeyString leftString && right instanceof MonkeyString rightString) {
            return evalStringInfixExpression(infixExpression, leftString, rightString);
        }

        // We support string concatenation even if only one side is a String. A change from the spec
        if (operation == TokenType.PLUS && (left instanceof MonkeyString || right instanceof MonkeyString)) {
            return new MonkeyString(left.inspect() + right.inspect());
        }

        return switch (operation) {
            case EQUAL -> MonkeyBoolean.nativeToMonkey(left == right);
            case NOT_EQUAL -> MonkeyBoolean.nativeToMonkey(left != right);

            default -> {
                // ¯\_(ツ)_/¯ A change from the spec
                if (left == MonkeyNull.INSTANCE || right == MonkeyNull.INSTANCE) {
                    yield evalNullInfixExpression(infixExpression, left, right);
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

    // We only evaluate the right side if necessary
    private MonkeyObject<?> evalBooleanLogicOperation(InfixExpression infixExpression, MonkeyObject<?> left) throws EvaluationException {
        return switch (infixExpression.getToken().type()) {
            case AND -> {
                if (isTruthy(left)) {
                    MonkeyObject<?> right = eval(infixExpression.getRight());
                    yield MonkeyBoolean.nativeToMonkey(isTruthy(right));
                }

                yield MonkeyBoolean.FALSE;
            }
            case OR -> {
                if (isTruthy(left)) {
                    yield MonkeyBoolean.TRUE;
                }

                MonkeyObject<?> right = eval(infixExpression.getRight());
                yield MonkeyBoolean.nativeToMonkey(isTruthy(right));
            }

            // Should not be thrown by using the language
            default ->
                    throw new IllegalStateException("INTERPRETER BUG: " + infixExpression.getToken().type() + " is not a Boolean logic operator");
        };
    }

    private MonkeyObject<?> evalNullInfixExpression(
            InfixExpression infixExpression,
            MonkeyObject<?> left,
            MonkeyObject<?> right
    ) throws EvaluationException {
        return switch (infixExpression.getToken().type()) {
            case PLUS, MINUS, ASTERISK, SLASH -> MonkeyNull.INSTANCE;
            default -> {
                var detail = left == right ? "both values are" : left == MonkeyNull.INSTANCE ? "left value is" : "right value is";
                throw new EvaluationException(infixExpression.getToken(), "Null value error: %s null", detail);
            }
        };
    }

    private MonkeyObject<?> evalStringInfixExpression(
            InfixExpression infixExpression,
            MonkeyString leftString,
            MonkeyString rightString
    ) throws EvaluationException {
        return switch (infixExpression.getToken().type()) {
            case PLUS -> new MonkeyString(leftString.getValue() + rightString.getValue());
            case EQUAL -> MonkeyBoolean.nativeToMonkey(leftString.getValue().equals(rightString.getValue()));
            case NOT_EQUAL -> MonkeyBoolean.nativeToMonkey(!leftString.getValue().equals(rightString.getValue()));
            case GT -> MonkeyBoolean.nativeToMonkey(leftString.getValue().compareTo(rightString.getValue()) > 0);
            case LT -> MonkeyBoolean.nativeToMonkey(leftString.getValue().compareTo(rightString.getValue()) < 0);

            default -> throw new EvaluationException(
                    infixExpression.getToken(),
                    "Operation %s not supported between Strings",
                    infixExpression.getOperator()
            );
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
                    throw new IllegalStateException("INTERPRETER BUG: Unexpected value (unreachable code): " + infixExpression.getToken().type());
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

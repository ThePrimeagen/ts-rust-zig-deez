package root.evaluation.objects.impl;

import root.ast.expressions.FunctionLiteralExpression;
import root.evaluation.Environment;
import root.evaluation.EvaluationException;
import root.evaluation.Evaluator;
import root.evaluation.objects.MonkeyFunctionInterface;
import root.evaluation.objects.MonkeyObject;
import root.evaluation.objects.ObjectType;

public class MonkeyFunction extends MonkeyObject<MonkeyFunctionInterface> {

    private final FunctionLiteralExpression functionLiteral;

    public MonkeyFunction(Environment creationEnv, FunctionLiteralExpression functionLiteral) {
        super(ObjectType.FUNCTION);
        this.functionLiteral = functionLiteral;

        setValue((evaluator, expressions) -> {
            var parameters = functionLiteral.getParameters();
            var body = functionLiteral.getBody();

            if (parameters.size() != expressions.size()) {
                throw new EvaluationException(
                        functionLiteral.getToken(),
                        "Wrong number of arguments. Expected %d, got %d",
                        parameters.size(),
                        expressions.size()
                );
            }

            var environment = new Environment(creationEnv);

            for (int i = 0; i < parameters.size(); i++) {
                MonkeyObject<?> parameterValue = evaluator.eval(expressions.get(i));
                environment.set(parameters.get(i).getValue(), parameterValue);
            }

            var functionEvaluator = new Evaluator(environment);

            return functionEvaluator.eval(body);
        });
    }

    public FunctionLiteralExpression getFunctionLiteral() {
        return functionLiteral;
    }

    @Override
    public String inspect() {
        return functionLiteral.toString();
    }
}

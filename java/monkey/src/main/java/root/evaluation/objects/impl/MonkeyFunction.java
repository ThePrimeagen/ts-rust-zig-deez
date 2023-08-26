package root.evaluation.objects.impl;

import root.ast.expressions.FunctionLiteralExpression;
import root.evaluation.Environment;
import root.evaluation.Evaluator;
import root.evaluation.objects.AbstractMonkeyFunction;
import root.evaluation.objects.ObjectType;

public class MonkeyFunction extends AbstractMonkeyFunction {

    private final FunctionLiteralExpression functionLiteral;

    public MonkeyFunction(Environment creationEnv, FunctionLiteralExpression functionLiteral) {
        super(ObjectType.FUNCTION);
        this.functionLiteral = functionLiteral;

        setValue((callToken, arguments) -> {
            var parameters = functionLiteral.getParameters();
            var body = functionLiteral.getBody();

            checkArgumentCount(callToken, parameters.size(), arguments.size());

            var environment = new Environment(creationEnv);

            for (int i = 0; i < parameters.size(); i++) {
                environment.set(parameters.get(i).getValue(), arguments.get(i));
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

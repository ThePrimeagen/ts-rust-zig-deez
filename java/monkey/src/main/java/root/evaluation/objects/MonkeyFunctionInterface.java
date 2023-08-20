package root.evaluation.objects;

import root.ast.expressions.Expression;
import root.evaluation.EvaluationException;
import root.evaluation.Evaluator;

import java.util.List;

@FunctionalInterface
public interface MonkeyFunctionInterface {

    MonkeyObject<?> apply(Evaluator evaluator, List<Expression> arguments) throws EvaluationException;
}

package root.evaluation.objects;

import root.LocalizedToken;
import root.evaluation.EvaluationException;

import java.util.List;

@FunctionalInterface
public interface MonkeyFunctionInterface {

    MonkeyObject<?> apply(LocalizedToken callToken, List<MonkeyObject<?>> arguments) throws EvaluationException;
}

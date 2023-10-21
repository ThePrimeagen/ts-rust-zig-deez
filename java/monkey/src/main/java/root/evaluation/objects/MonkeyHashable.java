package root.evaluation.objects;

import root.LocalizedToken;
import root.evaluation.EvaluationException;

public interface MonkeyHashable {

    HashKey hashKey();

    static MonkeyHashable checkIsHashable(MonkeyObject<?> object, LocalizedToken callToken) throws EvaluationException {
        if (object instanceof MonkeyHashable hashable) {
            return hashable;
        }

        throw new EvaluationException(callToken, "Index to an hash must be an Expression that yields an Int, String or Boolean");
    }
}

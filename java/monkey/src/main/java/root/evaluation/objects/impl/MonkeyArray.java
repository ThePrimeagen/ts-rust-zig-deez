package root.evaluation.objects.impl;

import root.LocalizedToken;
import root.evaluation.EvaluationException;
import root.evaluation.objects.MonkeyObject;
import root.evaluation.objects.ObjectType;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public class MonkeyArray extends MonkeyObject<List<MonkeyObject<?>>> {

    public MonkeyArray(List<MonkeyObject<?>> elements) {
        super(ObjectType.ARRAY);
        setValue(Collections.unmodifiableList(elements));
    }

    @Override
    public String inspect() {
        return "[%s]".formatted(getValue().stream().map(MonkeyObject::inspect).collect(Collectors.joining(", ")));
    }

    public static MonkeyInteger verifyIndexIsInteger(MonkeyObject<?> index, LocalizedToken indexToken) throws EvaluationException {
        if (index instanceof MonkeyInteger integer) {
            return integer;
        }

        throw new EvaluationException(indexToken, "Index to an array must be an Expression that yields an Int");
    }
}

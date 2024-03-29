package root.evaluation.objects.impl;

import root.evaluation.objects.HashKey;
import root.evaluation.objects.MonkeyObject;
import root.evaluation.objects.ObjectType;

import java.util.Collections;
import java.util.Map;
import java.util.stream.Collectors;

public class MonkeyHash extends MonkeyObject<Map<HashKey, MonkeyObject<?>>> {

    public MonkeyHash(Map<HashKey, MonkeyObject<?>> pairs) {
        super(ObjectType.HASH);
        setValue(Collections.unmodifiableMap(pairs));
    }

    @Override
    public String inspect() {
        var pairsString = getValue()
                .entrySet()
                .stream()
                .map(pair -> "%s : %s".formatted(pair.getKey().getOriginalObject().inspect(), pair.getValue().inspect()))
                .collect(Collectors.joining(", "));

        return "{%s}".formatted(pairsString);
    }
}

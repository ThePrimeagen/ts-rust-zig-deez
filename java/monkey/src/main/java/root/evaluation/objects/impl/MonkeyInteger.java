package root.evaluation.objects.impl;

import root.evaluation.objects.HashKey;
import root.evaluation.objects.MonkeyHashable;
import root.evaluation.objects.MonkeyObject;
import root.evaluation.objects.ObjectType;

public class MonkeyInteger extends MonkeyObject<Long> implements MonkeyHashable {

    public MonkeyInteger(long value) {
        super(ObjectType.INTEGER);
        setValue(value);
    }

    @Override
    public String inspect() {
        return String.valueOf(getValue());
    }

    @Override
    public HashKey hashKey() {
        return new HashKey(this);
    }
}

package root.evaluation.objects.impl;

import root.evaluation.objects.MonkeyObject;
import root.evaluation.objects.ObjectType;

public class MonkeyInteger extends MonkeyObject<Long> {

    public MonkeyInteger(long value) {
        super(ObjectType.INTEGER);
        setValue(value);
    }

    @Override
    public String inspect() {
        return String.valueOf(getValue());
    }
}

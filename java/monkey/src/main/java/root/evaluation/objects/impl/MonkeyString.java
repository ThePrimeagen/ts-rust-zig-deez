package root.evaluation.objects.impl;

import root.evaluation.objects.HashKey;
import root.evaluation.objects.MonkeyHashable;
import root.evaluation.objects.MonkeyObject;
import root.evaluation.objects.ObjectType;

public class MonkeyString extends MonkeyObject<String> implements MonkeyHashable {

    public MonkeyString(String value) {
        super(ObjectType.STRING);
        setValue(value);
    }

    @Override
    public String inspect() {
        return getValue();
    }

    @Override
    public HashKey hashKey() {
        return new HashKey(this);
    }
}

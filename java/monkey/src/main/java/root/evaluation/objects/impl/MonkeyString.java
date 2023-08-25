package root.evaluation.objects.impl;

import root.evaluation.objects.MonkeyObject;
import root.evaluation.objects.ObjectType;

public class MonkeyString extends MonkeyObject<String> {

    public MonkeyString(String value) {
        super(ObjectType.STRING);
        setValue(value);
    }

    @Override
    public String inspect() {
        return getValue();
    }
}

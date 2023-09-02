package root.evaluation.objects.impl;

import root.evaluation.objects.HashKey;
import root.evaluation.objects.MonkeyHashable;
import root.evaluation.objects.MonkeyObject;
import root.evaluation.objects.ObjectType;

public class MonkeyBoolean extends MonkeyObject<Boolean> implements MonkeyHashable {

    public static final MonkeyBoolean TRUE = new MonkeyBoolean(true);

    public static final MonkeyBoolean FALSE = new MonkeyBoolean(false);

    private MonkeyBoolean(boolean value) {
        super(ObjectType.BOOLEAN);
        setValue(value);
    }

    public static MonkeyBoolean nativeToMonkey(boolean bool) {
        return bool ? TRUE : FALSE;
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

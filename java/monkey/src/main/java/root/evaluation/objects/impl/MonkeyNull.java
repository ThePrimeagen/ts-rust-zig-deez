package root.evaluation.objects.impl;

import root.evaluation.objects.MonkeyObject;
import root.evaluation.objects.ObjectType;

import javax.lang.model.type.NullType;

public class MonkeyNull extends MonkeyObject<NullType> {

    public static final MonkeyNull INSTANCE = new MonkeyNull();

    private MonkeyNull() {
        super(ObjectType.NULL);
    }

    @Override
    public String inspect() {
        return "null";
    }
}

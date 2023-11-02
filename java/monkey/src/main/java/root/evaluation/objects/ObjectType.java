package root.evaluation.objects;

import root.evaluation.objects.impl.*;

import java.util.Arrays;

public enum ObjectType {

    NULL(MonkeyReturn.class),
    BOOLEAN(MonkeyBoolean.class),
    INTEGER(MonkeyInteger.class),
    RETURN_VALUE_OBJ(MonkeyReturn.class),
    FUNCTION(MonkeyFunction.class),
    BUILTIN_OBJ(BuiltinFunction.class),
    STRING(MonkeyString.class),
    ARRAY(MonkeyArray.class),
    HASH(MonkeyHash.class);

    private final Class<?> monkeyClass;

    ObjectType(Class<?> monkeyClass) {
        this.monkeyClass = monkeyClass;
    }

    public static ObjectType getTypeFromClass(Class<?> tClass) {
        return Arrays.stream(values()).filter(it -> it.monkeyClass == tClass).findFirst().orElseThrow();
    }
}

package root.evaluation.objects.impl;

import root.evaluation.objects.AbstractMonkeyFunction;
import root.evaluation.objects.MonkeyFunctionInterface;
import root.evaluation.objects.ObjectType;

public class BuiltinFunction extends AbstractMonkeyFunction {

    public BuiltinFunction(MonkeyFunctionInterface functionInterface) {
        super(ObjectType.BUILTIN_OBJ);
        setValue(functionInterface);
    }

    @Override
    public String inspect() {
        return "builtin function";
    }
}

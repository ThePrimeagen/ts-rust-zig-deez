package root.evaluation;

import root.evaluation.objects.AbstractMonkeyFunction;
import root.evaluation.objects.MonkeyObject;
import root.evaluation.objects.impl.BuiltinFunction;
import root.evaluation.objects.impl.MonkeyInteger;
import root.evaluation.objects.impl.MonkeyUnit;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public enum BuiltinFunctions {

    PUTS("puts", new BuiltinFunction((callToken, arguments) -> {
        BuiltinFunctions.printArgs(arguments);
        System.out.println();

        return MonkeyUnit.INSTANCE;
    })),
    PUTS_NO_NEWLINE("putsNoln", new BuiltinFunction(((callToken, arguments) -> {
        BuiltinFunctions.printArgs(arguments);

        return MonkeyUnit.INSTANCE;
    }))),
    MILI_TIME("miliTime", new BuiltinFunction(((callToken, arguments) -> {
        if (!arguments.isEmpty()) {
            AbstractMonkeyFunction.throwWorngNumberOfArgumentsError(callToken, 0, arguments.size());
        }

        return new MonkeyInteger(System.currentTimeMillis());
    }))),
    NANO_TIME("nanoTime", new BuiltinFunction(((callToken, arguments) -> {
        if (!arguments.isEmpty()) {
            AbstractMonkeyFunction.throwWorngNumberOfArgumentsError(callToken, 0, arguments.size());
        }

        return new MonkeyInteger(System.nanoTime());
    })));

    private final String identifier;
    private final BuiltinFunction builtinFunction;

    private static Map<String, BuiltinFunction> functionsMap;

    BuiltinFunctions(String identifier, BuiltinFunction builtinFunction) {
        this.identifier = identifier;
        this.builtinFunction = builtinFunction;
    }

    public static Optional<BuiltinFunction> getFunction(String identifier) {
        if (functionsMap == null) {
            functionsMap = new HashMap<>();

            for (var entry : values()) {
                if (functionsMap.containsKey(entry.identifier)) {
                    throw new IllegalStateException("Function %s already declared".formatted(entry.identifier));
                }
                functionsMap.put(entry.identifier, entry.builtinFunction);
            }
        }

        return Optional.ofNullable(functionsMap.get(identifier));
    }

    private static void printArgs(List<MonkeyObject<?>> arguments) {
        var first = true;
        for (var arg : arguments) {
            if (first) {
                first = false;
            } else {
                System.out.print(" ");
            }

            System.out.print(arg.inspect());
        }
    }
}

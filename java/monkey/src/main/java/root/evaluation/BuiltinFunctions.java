package root.evaluation;

import root.evaluation.objects.AbstractMonkeyFunction;
import root.evaluation.objects.MonkeyFunctionInterface;
import root.evaluation.objects.MonkeyObject;
import root.evaluation.objects.ObjectType;
import root.evaluation.objects.impl.*;

import java.util.*;

public enum BuiltinFunctions {

    PUTS("puts", (callToken, arguments) -> {
        BuiltinFunctions.printArgs(arguments);
        System.out.println();

        return MonkeyUnit.INSTANCE;
    }),
    PUTS_NO_NEWLINE("putsNoln", (callToken, arguments) -> {
        BuiltinFunctions.printArgs(arguments);

        return MonkeyUnit.INSTANCE;
    }),
    MILI_TIME("miliTime", (callToken, arguments) -> {
        AbstractMonkeyFunction.checkArgumentCount(callToken, 0, arguments.size());

        return new MonkeyInteger(System.currentTimeMillis());
    }),
    NANO_TIME("nanoTime", (callToken, arguments) -> {
        AbstractMonkeyFunction.checkArgumentCount(callToken, 0, arguments.size());

        return new MonkeyInteger(System.nanoTime());
    }),
    LEN("len", (callToken, arguments) -> {
        AbstractMonkeyFunction.checkArgumentCount(callToken, 1, arguments.size());
        MonkeyObject<?> argument = arguments.get(0);

        // This function will also accept Objects
        return switch (argument) {
            case MonkeyString string -> new MonkeyInteger(string.getValue().length());
            case MonkeyArray array -> new MonkeyInteger(array.getValue().size());
            default ->
                    throw new EvaluationException(callToken, "Argument to `len` not supported, got %s", argument.getType());
        };
    }),
    UPPERCASE("uppercase", (callToken, arguments) -> {
        AbstractMonkeyFunction.checkArgumentCount(callToken, 1, arguments.size());
        AbstractMonkeyFunction.checkArgumentType(callToken, arguments.get(0), ObjectType.STRING, "uppercase");

        return new MonkeyString(((MonkeyString) arguments.get(0)).getValue().toUpperCase());
    }),
    LOWERCASE("lowercase", (callToken, arguments) -> {
        AbstractMonkeyFunction.checkArgumentCount(callToken, 1, arguments.size());
        AbstractMonkeyFunction.checkArgumentType(callToken, arguments.get(0), ObjectType.STRING, "lowercase");

        return new MonkeyString(((MonkeyString) arguments.get(0)).getValue().toLowerCase());
    }),
    FIRST("first", (callToken, arguments) -> {
        AbstractMonkeyFunction.checkArgumentCount(callToken, 1, arguments.size());
        AbstractMonkeyFunction.checkArgumentType(callToken, arguments.get(0), ObjectType.ARRAY, "first");

        var array = (MonkeyArray) arguments.get(0);

        if (array.getValue().isEmpty()) {
            return MonkeyNull.INSTANCE;
        }
        return array.getValue().get(0);
    }),
    LAST("last", (callToken, arguments) -> {
        AbstractMonkeyFunction.checkArgumentCount(callToken, 1, arguments.size());
        AbstractMonkeyFunction.checkArgumentType(callToken, arguments.get(0), ObjectType.ARRAY, "last");

        var array = (MonkeyArray) arguments.get(0);

        if (array.getValue().isEmpty()) {
            return MonkeyNull.INSTANCE;
        }
        return array.getValue().get(array.getValue().size() - 1);
    }),
    REST("rest", (callToken, arguments) -> {
        AbstractMonkeyFunction.checkArgumentCount(callToken, 1, arguments.size());
        AbstractMonkeyFunction.checkArgumentType(callToken, arguments.get(0), ObjectType.ARRAY, "rest");

        var array = (MonkeyArray) arguments.get(0);

        if (array.getValue().isEmpty()) {
            return MonkeyNull.INSTANCE;
        }

        var newArray = new ArrayList<MonkeyObject<?>>();

        for (int i = 1; i < array.getValue().size(); i++) {
            newArray.add(array.getValue().get(i));
        }

        return new MonkeyArray(newArray);
    }),
    PUSH("push", (callToken, arguments) -> {
        AbstractMonkeyFunction.checkArgumentCount(callToken, 2, arguments.size());
        AbstractMonkeyFunction.checkArgumentType(callToken, arguments.get(0), ObjectType.ARRAY, "rest");

        var array = (MonkeyArray) arguments.get(0);

        var elementsCopy = new ArrayList<>(array.getValue());
        elementsCopy.add(arguments.get(1));

        return new MonkeyArray(elementsCopy);
    });

    private final String identifier;
    private final BuiltinFunction builtinFunction;

    private static Map<String, BuiltinFunction> functionsMap;

    BuiltinFunctions(String identifier, MonkeyFunctionInterface functionalInterface) {
        this.identifier = identifier;
        this.builtinFunction = new BuiltinFunction(functionalInterface);
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

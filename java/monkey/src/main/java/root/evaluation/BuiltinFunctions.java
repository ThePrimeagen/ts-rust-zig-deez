package root.evaluation;

import root.LocalizedToken;
import root.evaluation.objects.AbstractMonkeyFunction;
import root.evaluation.objects.MonkeyFunctionInterface;
import root.evaluation.objects.MonkeyObject;
import root.evaluation.objects.impl.BuiltinFunction;
import root.evaluation.objects.impl.MonkeyInteger;
import root.evaluation.objects.impl.MonkeyString;
import root.evaluation.objects.impl.MonkeyUnit;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

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
        if (!arguments.isEmpty()) {
            AbstractMonkeyFunction.throwWorngNumberOfArgumentsError(callToken, 0, arguments.size());
        }

        return new MonkeyInteger(System.currentTimeMillis());
    }),
    NANO_TIME("nanoTime", (callToken, arguments) -> {
        if (!arguments.isEmpty()) {
            AbstractMonkeyFunction.throwWorngNumberOfArgumentsError(callToken, 0, arguments.size());
        }

        return new MonkeyInteger(System.nanoTime());
    }),
    LEN("len", (callToken, arguments) -> {
        if (arguments.size() != 1) {
            AbstractMonkeyFunction.throwWorngNumberOfArgumentsError(callToken, 1, arguments.size());
        }
        MonkeyObject<?> argument = arguments.get(0);

        // This function will also accept Arrays and Objects
        return switch (argument) {
            case MonkeyString string -> new MonkeyInteger(string.getValue().length());
            default ->
                    throw new EvaluationException(callToken, "Argument to `len` not supported, got %s", argument.getType());
        };
    }),
    UPPERCASE("uppercase", (callToken, arguments) ->
            new MonkeyString(checkSingleStringArgument(callToken, arguments, "uppercase").getValue().toUpperCase())),
    LOWERCASE("lowercase", (callToken, arguments) ->
            new MonkeyString(checkSingleStringArgument(callToken, arguments, "lowercase").getValue().toLowerCase()));

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

    private static MonkeyString checkSingleStringArgument(
            LocalizedToken callToken,
            List<MonkeyObject<?>> arguments,
            String functionName
    ) throws EvaluationException {
        if (arguments.size() != 1) {
            AbstractMonkeyFunction.throwWorngNumberOfArgumentsError(callToken, 1, arguments.size());
        }
        MonkeyObject<?> argument = arguments.get(0);

        if (argument instanceof MonkeyString string) {
            return string;
        }

        throw new EvaluationException(
                callToken,
                "Argument to %s not supported, got %s. Only Strings are accepted",
                functionName,
                argument.getType()
        );
    }
}

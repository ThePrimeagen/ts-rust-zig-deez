package root.evaluation;

import root.evaluation.objects.MonkeyObject;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public class Environment {

    private final Map<String, MonkeyObject<?>> bindings = new HashMap<>();

    private final Environment outer;

    public Environment() {
        outer = null;
    }

    public Environment(Environment outer) {
        this.outer = outer;
    }

    public <T> MonkeyObject<T> set(String name, MonkeyObject<T> value) {
        bindings.put(name, value);
        return value;
    }

    public Optional<MonkeyObject<?>> get(String name) {
        var value = bindings.get(name);

        if (value == null) {
            if (outer == null) {
                return Optional.empty();
            }

            return outer.get(name);
        }

        return Optional.of(value);
    }
}

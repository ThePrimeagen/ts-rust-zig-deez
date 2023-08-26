package root.evaluation.objects;

import java.util.Objects;

public abstract class MonkeyObject<T> {

    private final ObjectType type;

    private T value;

    public MonkeyObject(ObjectType type) {
        this.type = type;
    }

    public abstract String inspect();

    public ObjectType getType() {
        return type;
    }

    public T getValue() {
        return value;
    }

    protected void setValue(T value) {
        this.value = Objects.requireNonNull(value);
    }

    @Override
    public String toString() {
        return getClass().getSimpleName() + " " + inspect();
    }
}

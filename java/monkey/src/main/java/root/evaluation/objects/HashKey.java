package root.evaluation.objects;

import java.util.Objects;

public class HashKey {

    private final ObjectType type;

    private final int baseHash;

    private final MonkeyObject<?> originalObject;

    public HashKey(MonkeyObject<?> object) {
        this.type = object.getType();
        this.baseHash = object.getValue().hashCode();
        this.originalObject = object;
    }

    public MonkeyObject<?> getOriginalObject() {
        return originalObject;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        HashKey hashKey = (HashKey) o;
        return baseHash == hashKey.baseHash && type == hashKey.type;
    }

    @Override
    public int hashCode() {
        return Objects.hash(type, baseHash);
    }
}

package root.evaluation.objects;

import java.util.Objects;

public class HashKey {

    private final ObjectType type;

    private final int baseHash;

    private final String keyStringRep;

    public HashKey(MonkeyObject<?> object) {
        this.type = object.getType();
        this.baseHash = object.getValue().hashCode();
        this.keyStringRep = object.inspect();
    }

    public String getKeyStringRep() {
        return keyStringRep;
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

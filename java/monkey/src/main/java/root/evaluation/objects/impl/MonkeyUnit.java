package root.evaluation.objects.impl;

import root.evaluation.objects.MonkeyObject;
import root.evaluation.objects.ObjectType;

/**
 * This is not in the original Monkey spec, but I feel it is a valid addition. It is equivalent to Rust's unit (),
 * undefined in JS or to void in most other languages. I added it, so you can have a function that returns no value
 * without having to return null, which is ambiguous. The only way to produce this value is with the now valid "return;"
 * syntax or by calling a function that returns unit. It cannot be assigned to a variable and trying to do so will
 * result in an Evaluation Error
 */
public class MonkeyUnit extends MonkeyObject<Void> {

    public static final MonkeyUnit INSTANCE = new MonkeyUnit();

    private MonkeyUnit() {
        super(ObjectType.UNIT);
    }

    @Override
    public String inspect() {
        return "";
    }
}

package root.ast.expressions;

import root.LocalizedToken;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public class HashLiteralExpression extends Expression {

    public record KeyValuePair(Expression key, Expression value){
    };

    // No need to save the pairs as a Map here, we will only read it once when evaluating and don't need to search
    // values by keys. It would be hard and kinda pointless writing Hash implementations for all expressions
    private final List<KeyValuePair> pairs;

    public HashLiteralExpression(LocalizedToken token, List<KeyValuePair> pairs) {
        super(token);
        this.pairs = Collections.unmodifiableList(pairs);
    }

    public List<KeyValuePair> getPairs() {
        return pairs;
    }

    @Override
    public String stringRep() {
        var pairsString = pairs
                .stream()
                .map(pair -> "%s : %s".formatted(pair.key.stringRep(), pair.value.stringRep()))
                .collect(Collectors.joining(", "));

        return "{%s}".formatted(pairsString);
    }
}

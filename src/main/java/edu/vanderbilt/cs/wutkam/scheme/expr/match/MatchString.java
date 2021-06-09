package edu.vanderbilt.cs.wutkam.scheme.expr.match;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

import java.util.HashSet;
import java.util.Set;

/**
 */
public class MatchString implements Match {
    public String value;
    protected Set<String> stringsSeen;

    public MatchString(String value) {
        this.value = value;
    }

    @Override
    public boolean matches(Expression expression) {
        return ((MatchString)expression).value.equals(value);
    }

    @Override
    public void defineEnvironment(Expression expr, Environment<Expression> env) {

    }

    @Override
    public void unify(TypeRef matchTargetType, Environment<TypeRef> env) throws LispException {

    }

    protected void saw(String str) {
        if (stringsSeen == null) stringsSeen = new HashSet<>();
        stringsSeen.add(str);
    }

    protected static final String[] tryStrings = new String[] { "foo", "bar", "baz", "quux", "qux", "corge", "grault",
        "garply", "waldo", "fred", "plugh", "xyxxy", "thud" };

    protected String getUnseen() {
        if (stringsSeen == null) return tryStrings[0];
        for (String s: tryStrings) {
            if (!stringsSeen.contains(s)) return s;
        }
        int i=0;
        while (stringsSeen.contains(tryStrings[i%tryStrings.length]+i)) i++;
        return tryStrings[i%tryStrings.length]+i;
    }

    @Override
    public String toString() { return (new StringExpr(value)).toString(); }
}

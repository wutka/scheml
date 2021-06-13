package edu.vanderbilt.cs.wutkam.scheme.expr.match;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

/** A match pattern for strings
 */
public class MatchString implements Match {
    public String value;

    public MatchString(String value) {
        this.value = value;
    }

    @Override
    public boolean matches(Expression expression) {
        return ((StringExpr)expression).value.equals(value);
    }

    @Override
    public void defineEnvironment(Expression expr, Environment<Expression> env) {
        // A string contains no variables so there is nothing to match
        // A potential future enhancement here might be to support regex matches
        // with named groups and set those in the environment here
        // The regex match could make it harder to do exhaustiveness checking, though
    }

    @Override
    public void unify(TypeRef matchTargetType, Environment<TypeRef> env) throws LispException {

    }

    @Override
    public String toString() { return (new StringExpr(value)).toString(); }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        MatchString that = (MatchString) o;
        return value.equals(that.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }
}

package edu.vanderbilt.cs.wutkam.scheme.expr.match;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.IntExpr;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.IntType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

/** A match pattern for ints
 */
public class MatchInt implements Match {
    public long value;

    public MatchInt(long value) {
        this.value = value;
    }

    @Override
    public boolean matches(Expression expression) {
        return ((IntExpr)expression).value == value;
    }

    @Override
    public void defineEnvironment(Expression expr, Environment<Expression> env) {
        // An int contains no variable so there is nothing to define
    }

    @Override
    public void unify(TypeRef matchTargetType, Environment<TypeRef> env) throws LispException {
        matchTargetType.unify(new TypeRef(IntType.TYPE));
    }

    @Override
    public String toString() { return (new IntExpr(value)).toString(); }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        MatchInt matchInt = (MatchInt) o;
        return value == matchInt.value;
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }
}

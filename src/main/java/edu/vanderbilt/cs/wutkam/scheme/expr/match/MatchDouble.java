package edu.vanderbilt.cs.wutkam.scheme.expr.match;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.DoubleExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.DoubleType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

/** A match pattern for doubles
 */
public class MatchDouble implements Match {
    public double value;

    public MatchDouble(double value) {
        this.value = value;
    }

    @Override
    public boolean matches(Expression expression) {
        return ((DoubleExpr)expression).value == value;
    }

    @Override
    public void defineEnvironment(Expression expr, Environment<Expression> env) {
        // There are no variables in a double so there is nothing to match
    }

    @Override
    public void unify(TypeRef matchTargetType, Environment<TypeRef> env) throws LispException {
        matchTargetType.unify(new TypeRef(DoubleType.TYPE));
    }

    @Override
    public String toString() { return (new DoubleExpr(value)).toString(); }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        MatchDouble that = (MatchDouble) o;
        return Double.compare(that.value, value) == 0;
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }
}

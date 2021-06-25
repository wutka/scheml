package edu.vanderbilt.cs.wutkam.scheml.expr.match;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.BoolExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.BooleanType;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;

import java.util.Objects;

/** A match pattern for a boolean
 */
public class MatchBool implements Match {
    boolean value;

    public MatchBool(boolean value) {
        this.value = value;
    }

    @Override
    public boolean matches(Expression expression) {
        return ((BoolExpr)expression).value == value;
    }

    @Override
    public void defineEnvironment(Expression expr, Environment<Expression> env) {
        // A bool doesn't contain any variables so there is nothing to define
    }

    @Override
    public void unify(TypeRef matchTargetType, Environment<TypeRef> env) throws LispException {
        matchTargetType.unify(new TypeRef(BooleanType.TYPE));
    }

    @Override
    public String toString() { return (new BoolExpr(value)).toString(); }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        MatchBool matchBool = (MatchBool) o;
        return value == matchBool.value;
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }
}

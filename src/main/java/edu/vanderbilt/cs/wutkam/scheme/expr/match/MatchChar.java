package edu.vanderbilt.cs.wutkam.scheme.expr.match;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.CharExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.CharType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

/** A match pattern for a character
 */
public class MatchChar implements Match {
    public char value;

    public MatchChar(char value) {
        this.value = value;
    }

    @Override
    public boolean matches(Expression expression) {
        return ((CharExpr) expression).value == value;
    }

    @Override
    public void defineEnvironment(Expression expr, Environment<Expression> env) {
        // A character doesn't have any variables so there is nothing to define
    }

    @Override
    public void unify(TypeRef matchTargetType, Environment<TypeRef> env) throws LispException {
        matchTargetType.unify(new TypeRef(CharType.TYPE));
    }

    @Override
    public String toString() { return (new CharExpr(value)).toString(); }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        MatchChar matchChar = (MatchChar) o;
        return value == matchChar.value;
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }
}

package edu.vanderbilt.cs.wutkam.scheml.expr.match;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.SymbolExpr;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;

import java.util.Objects;

/** A match pattern for symbols
 */
public class MatchSymbol implements Match {
    public SymbolExpr value;

    public MatchSymbol(SymbolExpr value) {
        this.value = value;
    }

    @Override
    public boolean matches(Expression expression) {
        return ((SymbolExpr)expression).equals(value);
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
    public Expression toScheml() {
        return value;
    }

    @Override
    public String toString() { return value.toString(); }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        MatchSymbol that = (MatchSymbol) o;
        return value.equals(that.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }
}

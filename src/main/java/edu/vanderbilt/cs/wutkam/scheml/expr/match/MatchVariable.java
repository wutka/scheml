package edu.vanderbilt.cs.wutkam.scheml.expr.match;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.SymbolExpr;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;

import java.util.Objects;

/** Represents a variable name or _ in a match
 */
public class MatchVariable implements Match {
    public String name;

    public MatchVariable(String name) {
        this.name = name;
    }

    @Override
    public boolean matches(Expression expression) {
        // A variable always matches a value
        return true;
    }

    @Override
    public void defineEnvironment(Expression expr, Environment<Expression> env) {
        // This is the only matcher that actually puts anything in the environment
        // If the name isn't _, use this variable's name as the key in the environment
        if (!name.equals("_")) {
            env.define(name, expr);
        }
    }

    @Override
    public void unify(TypeRef matchTargetType, Environment<TypeRef> env) throws LispException {
        // As with defineEnvironment, if this isn't a _, store the target type in the environment
        if (!name.equals("_")) {
            env.define(name, matchTargetType);
        }
    }

    @Override
    public String toString() { return (new SymbolExpr(name)).toString(); }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        MatchVariable that = (MatchVariable) o;
        return name.equals(that.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }
}

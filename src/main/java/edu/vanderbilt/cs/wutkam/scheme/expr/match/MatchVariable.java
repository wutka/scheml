package edu.vanderbilt.cs.wutkam.scheme.expr.match;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.SymbolExpr;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

import java.util.Objects;

/**
 */
public class MatchVariable implements Match {
    public String name;

    public MatchVariable(String name) {
        this.name = name;
    }

    @Override
    public boolean matches(Expression expression) {
        return true;
    }

    @Override
    public void defineEnvironment(Expression expr, Environment<Expression> env) {
        if (!name.equals("_")) {
            env.define(name, expr);
        }
    }

    @Override
    public void unify(TypeRef matchTargetType, Environment<TypeRef> env) throws LispException {
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

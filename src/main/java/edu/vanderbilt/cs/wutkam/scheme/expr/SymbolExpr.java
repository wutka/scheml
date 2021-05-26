package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;

import java.util.Objects;

public class SymbolExpr implements Expression {
    public String value;

    public SymbolExpr(String value) {
        this.value = value;
    }

    public Expression evaluate(Environment<Expression> env) throws LispException {
        Expression expr = env.lookup(value);
        if (expr == null) {
            throw new LispException("Unknown symbol "+value);
        }
        return expr.evaluate(env);
    }

    @Override
    public String toString() {
        return value;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        SymbolExpr that = (SymbolExpr) o;
        return Objects.equals(value, that.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }
}

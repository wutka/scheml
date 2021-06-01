package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.BooleanType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheme.type.UnifyException;

import java.util.HashMap;
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
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        TypeRef myType = env.lookup(value);
        if (myType == null) {
            throw new UnifyException("Unknown symbol - " + value);
        }
        if (env.isTopLevel(value)) {
            TypeRef typeCopy = myType.copy(new HashMap<>());
            typeRef.unify(typeCopy);
        } else {
            typeRef.unify(myType);
        }
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

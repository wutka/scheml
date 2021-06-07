package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheme.type.UnifyException;

import java.util.HashMap;
import java.util.Objects;

/** Represents a symbol that is expected to be present in the evaluation environment */
public class SymbolExpr implements Expression {
    public String value;

    public SymbolExpr(String value) {
        this.value = value;
    }

    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        // Look the symbol up
        Expression expr = env.lookup(value);
        if (expr == null) {
            throw new LispException("Unknown symbol "+value);
        }
        // Evaluate the symbol's expression
        return expr.evaluate(env, false);
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        TypeRef myType = env.lookup(value);
        if (myType == null) {
            throw new UnifyException("Unknown symbol - " + value);
        }
        if (env.isTopLevel(value)) {
            // If the type expression comes from the top level, make a copy of it so we don't alter
            // what is in the top level. For example, the identify function id has type 'a -> 'a, but
            // if we unify it in a context of (id 5) we don't want original definition to acquire that
            // more limited signature.
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

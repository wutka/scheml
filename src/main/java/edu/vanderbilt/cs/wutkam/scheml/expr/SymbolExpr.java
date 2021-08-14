package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheml.type.UnifyException;

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
            expr = SchemlRuntime.getTopLevel().lookup(value);
            if (expr == null) {
                throw new LispException("Unknown symbol " + value);
            }
        }
        // Evaluate the symbol's expression
        return expr.evaluate(env, false);
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        TypeRef myType = env.lookup(value);
        boolean isTopLevel = false;
        if (myType == null) {
            myType = SchemlRuntime.getUnifyTopLevel().lookup(value);
            if (myType == null) {
                throw new UnifyException("Unknown symbol - " + value);
            }
            isTopLevel = true;
        }
        if (isTopLevel) {
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
    public Expression toScheml() {
        return new SymbolLiteralExpr(value);
    }
    @Override
    public String toString() {
        return value;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null) return false;
        if (o instanceof SymbolExpr) {
            return value.equals(((SymbolExpr)o).value);
        } else if (o instanceof SymbolLiteralExpr) {
            return value.equals(((SymbolLiteralExpr) o).value);
        } else {
            return false;
        }
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }
}

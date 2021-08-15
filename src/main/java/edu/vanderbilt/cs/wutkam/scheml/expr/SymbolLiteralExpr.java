package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;
import edu.vanderbilt.cs.wutkam.scheml.type.SymbolType;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheml.type.UnifyException;

import java.util.HashMap;
import java.util.Objects;

/** Represents a symbol that is expected to be present in the evaluation environment */
public class SymbolLiteralExpr implements Expression {
    public String value;

    public SymbolLiteralExpr(String value) {
        this.value = value;
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {

        TypeRef myType = new TypeRef(SymbolType.TYPE);
        myType.unify(typeRef);
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
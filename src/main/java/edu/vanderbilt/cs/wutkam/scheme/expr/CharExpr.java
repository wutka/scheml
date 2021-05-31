package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.CharType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

import java.util.Objects;

public class CharExpr implements Expression {
    public final char value;

    public CharExpr(char value) {
        this.value = value;
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        TypeRef myType = new TypeRef(CharType.TYPE);
        typeRef.unify(myType);
    }

    @Override
    public String toString() {
        return "#\\"+Character.toString(value);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CharExpr charExpr = (CharExpr) o;
        return value == charExpr.value;
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }
}

package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.IntType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

import java.util.Objects;

/** Represents a simple integer object */
public class IntExpr implements Expression {
    public final long value;

    public IntExpr(long value) {
        this.value = value;
    }

    @Override
    public Object toJavaValue() { return value; }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        TypeRef myType = new TypeRef(IntType.TYPE);
        typeRef.unify(myType);
    }

    @Override
    public String toString() {
        return Long.toString(value);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        IntExpr intExpr = (IntExpr) o;
        return value == intExpr.value;
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }
}

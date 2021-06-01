package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.BooleanType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

import java.util.Objects;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/24/21
 * Time: 3:18 PM
 */
public class BoolExpr implements Expression {
    public final boolean value;

    public BoolExpr(boolean value) {
        this.value = value;
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        TypeRef myType = new TypeRef(BooleanType.TYPE);
        typeRef.unify(myType);
    }

    @Override
    public Object toJavaValue() { return value; }

    @Override
    public String toString()
    {
        return value ? "#t" : "#f";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        BoolExpr boolExpr = (BoolExpr) o;
        return value == boolExpr.value;
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }
}

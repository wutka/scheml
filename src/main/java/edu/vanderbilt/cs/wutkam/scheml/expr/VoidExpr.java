package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheml.type.VoidType;

/** Represents a void type
 */
public class VoidExpr implements Expression {
    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        TypeRef myType = new TypeRef(VoidType.TYPE);
        typeRef.unify(myType);
    }

    @Override
    public String toString()
    {
        return "void";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        return o != null && getClass() == o.getClass();
    }

    @Override
    public int hashCode() {
        return 137;
    }
}

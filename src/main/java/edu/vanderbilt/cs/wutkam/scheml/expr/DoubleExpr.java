package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.DoubleType;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;

import java.util.Objects;

/** Represents a double value */
public class DoubleExpr implements Expression {
   public final double value;

   public DoubleExpr(double value) {
       this.value = value;
   }

   @Override
   public Object toJavaValue() { return value; }

   @Override
   public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
       TypeRef myType = new TypeRef(DoubleType.TYPE);
       typeRef.unify(myType);
   }

   @Override
   public String toString() {
       return Double.toString(value);
   }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        DoubleExpr that = (DoubleExpr) o;
        return Double.compare(that.value, value) == 0;
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }
}

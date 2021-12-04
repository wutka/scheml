package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.BignumType;
import edu.vanderbilt.cs.wutkam.scheml.type.DoubleType;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;

import java.math.BigInteger;
import java.util.Objects;

/** Represents a double value */
public class BignumExpr implements Expression {
   public final BigInteger value;

   public BignumExpr(BigInteger value) {
       this.value = value;
   }

   @Override
   public Object toJavaValue() { return value; }

   @Override
   public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
       TypeRef myType = new TypeRef(BignumType.TYPE);
       typeRef.unify(myType);
   }

   @Override
   public String toString() {
       return value.toString();
   }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        BignumExpr that = (BignumExpr) o;
        return value.equals(that);
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }
}

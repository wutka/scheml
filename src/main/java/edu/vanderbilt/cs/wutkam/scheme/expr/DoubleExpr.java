package edu.vanderbilt.cs.wutkam.scheme.expr;

import java.util.Objects;

public class DoubleExpr implements Expression {
   public final double value;

   public DoubleExpr(double value) {
       this.value = value;
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

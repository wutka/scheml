package edu.vanderbilt.cs.wutkam.scheme.expr;

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

package edu.vanderbilt.cs.wutkam.scheme.expr;

import java.util.Objects;

public class IntExpr implements Expression {
    public final int value;

    public IntExpr(int value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return Integer.toString(value);
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

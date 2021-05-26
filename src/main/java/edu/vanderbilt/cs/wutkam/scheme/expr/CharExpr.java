package edu.vanderbilt.cs.wutkam.scheme.expr;

import java.util.Objects;

public class CharExpr implements Expression {
    public final char value;

    public CharExpr(char value) {
        this.value = value;
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

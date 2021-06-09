package edu.vanderbilt.cs.wutkam.scheme.expr.match;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.DoubleExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.DoubleType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

import java.util.HashSet;
import java.util.Set;

/**
 */
public class MatchDouble implements Match {
    public double value;

    protected Set<Double> doublesSeen;

    public MatchDouble(double value) {
        this.value = value;
    }

    @Override
    public boolean matches(Expression expression) {
        return ((DoubleExpr)expression).value == value;
    }

    @Override
    public void defineEnvironment(Expression expr, Environment<Expression> env) {

    }

    @Override
    public void unify(TypeRef matchTargetType, Environment<TypeRef> env) throws LispException {
        matchTargetType.unify(new TypeRef(DoubleType.TYPE));
    }

    protected void saw(double d) {
        if (doublesSeen == null) {
            doublesSeen = new HashSet<>();
        }
        doublesSeen.add(d);
    }

    protected double getUnseen() {
        double d = 0.0;
        if (doublesSeen == null) return d;
        while (doublesSeen.contains(d)) d += 1.0;
        return d;
    }
    @Override
    public String toString() { return (new DoubleExpr(value)).toString(); }
}

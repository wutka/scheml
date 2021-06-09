package edu.vanderbilt.cs.wutkam.scheme.expr.match;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.IntExpr;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.IntType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

import java.util.HashSet;
import java.util.Set;

/**
 */
public class MatchInt implements Match {
    public int value;
    protected Set<Integer> intsSeen;

    public MatchInt(int value) {
        this.value = value;
    }

    @Override
    public boolean matches(Expression expression) {
        return ((IntExpr)expression).value == value;
    }

    @Override
    public void defineEnvironment(Expression expr, Environment<Expression> env) {

    }

    @Override
    public void unify(TypeRef matchTargetType, Environment<TypeRef> env) throws LispException {
        matchTargetType.unify(new TypeRef(IntType.TYPE));
    }

    protected void saw(int i) {
        if (intsSeen == null) intsSeen = new HashSet<>();
        intsSeen.add(i);
    }

    protected int getUnseen() {
        int i=0;
        if (intsSeen == null) return i;
        while (intsSeen.contains(i)) i++;
        return i;
    }

    @Override
    public String toString() { return (new IntExpr(value)).toString(); }
}

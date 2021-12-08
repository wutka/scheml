package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.set;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.SetExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;

import java.util.HashSet;
import java.util.Set;

/** Returns a set that is the union of the two arguments
 */
public class SetUnion extends BuiltinFunctionExpr {
    public SetUnion(String name) {
        super(name, "set 'a -> set 'a -> set 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        SetExpr setA = (SetExpr) args[0];
        SetExpr setB = (SetExpr) args[1];
        Set<Expression> newSet = new HashSet<>(setA.set);
        newSet.addAll(setB.set);
        return new SetExpr(newSet);
    }
}
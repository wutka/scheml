package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.set;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.SetExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;

import java.util.HashSet;
import java.util.Set;

/** Removes a value from a set and returns the set
 */
public class SetRemove extends BuiltinFunctionExpr {
    public SetRemove(String name) {
        super(name, "set 'a -> 'a -> set 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        SetExpr set = (SetExpr) args[0];
        Expression elem = args[1];
        Set<Expression> newSet = new HashSet<>(set.set);
        newSet.remove(elem);
        return new SetExpr(newSet);
    }
}
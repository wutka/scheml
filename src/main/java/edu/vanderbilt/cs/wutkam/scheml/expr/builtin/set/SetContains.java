package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.set;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.BoolExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.SetExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;

/** Finds a value in a dict
 */
public class SetContains extends BuiltinFunctionExpr {
    public SetContains(String name) {
        super(name, "set 'a -> 'a -> bool");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        SetExpr set = (SetExpr) args[0];
        Expression key = args[1];
        return new BoolExpr(set.set.contains(key));
    }
}
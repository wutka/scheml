package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.set;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.IntExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.SetExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;

/** Returns the number of entries in the set
 */
public class SetLen extends BuiltinFunctionExpr {
    public SetLen(String name) {
        super(name, "set 'a -> int");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        SetExpr set = (SetExpr) args[0];
        return new IntExpr(set.set.size());
    }
}
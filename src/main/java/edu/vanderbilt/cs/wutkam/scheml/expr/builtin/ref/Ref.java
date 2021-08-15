package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.ref;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.ArrayExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.IntExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.FailException;

/** Returns the value of a reference */
public class Ref extends BuiltinFunctionExpr {
    public Ref(String name) {
        super(name, "ref 'a -> 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        AbstractTypeExpr abstractType = (AbstractTypeExpr) args[0];
        return abstractType.values.get(0);
    }
}

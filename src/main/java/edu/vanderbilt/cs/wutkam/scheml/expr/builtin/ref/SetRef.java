package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.ref;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;

/** Sets the value of a reference */
public class SetRef extends BuiltinFunctionExpr {
    public SetRef(String name) {
        super(name, "ref 'a -> 'a -> ref 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        AbstractTypeExpr abstractType = (AbstractTypeExpr) args[0];
        abstractType.values.set(0, args[1]);
        return abstractType;
    }
}

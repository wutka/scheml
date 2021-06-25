package edu.vanderbilt.cs.wutkam.scheml.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.BoolExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;

/** Checks to see if a list is null
 */
public class Empty extends BuiltinFunctionExpr {
    public Empty(String name) {
        super(name, "cons 'a -> bool");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // Type unification will ensure that this is a ConsExpr
        AbstractTypeExpr cons = (AbstractTypeExpr) args[0];
        return new BoolExpr(cons.constructorName.equals("Nil"));
    }
}

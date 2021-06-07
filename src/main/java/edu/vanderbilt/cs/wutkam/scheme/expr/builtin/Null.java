package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.BoolExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.ConsExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;

/** Checks to see if a list is null
 */
public class Null extends BuiltinFunctionExpr {
    public Null(String name) {
        super(name, "cons 'a -> bool");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // Type unification will ensure that this is a ConsExpr
        ConsExpr cons = (ConsExpr) args[0];
        return new BoolExpr(cons.head == null);
    }
}

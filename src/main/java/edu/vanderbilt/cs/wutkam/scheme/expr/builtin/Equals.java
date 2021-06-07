package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.BoolExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;

/** Compare any two objects
 */
public class Equals extends BuiltinFunctionExpr {
    public Equals(String name) {
        super(name, "'a -> 'a -> bool");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        return new BoolExpr(args[0].equals(args[1]));
    }
}

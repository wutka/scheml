package edu.vanderbilt.cs.wutkam.scheml.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.BoolExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;

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

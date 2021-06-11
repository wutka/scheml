package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.VoidExpr;

/** Prints a string to stdout
 */
public class Fail extends BuiltinFunctionExpr {
    public Fail(String name) {
        super(name, "string -> 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        Expression expr = args[0];
        // Type unification will ensure the expr is a string
        throw new FailException(((StringExpr)expr).value);
    }
}

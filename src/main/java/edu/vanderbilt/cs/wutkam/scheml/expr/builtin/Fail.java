package edu.vanderbilt.cs.wutkam.scheml.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.StringExpr;

/** Fails with a message to the top level in the repl
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

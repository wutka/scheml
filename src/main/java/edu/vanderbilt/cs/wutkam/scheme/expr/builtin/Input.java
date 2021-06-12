package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.VoidExpr;
import edu.vanderbilt.cs.wutkam.scheme.runtime.SchemeRuntime;

/** Prints a string to stdout
 */
public class Input extends BuiltinFunctionExpr {
    public Input(String name) {
        super(name, "void -> string");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        return new StringExpr(SchemeRuntime.getRepl().readLine());
    }
}

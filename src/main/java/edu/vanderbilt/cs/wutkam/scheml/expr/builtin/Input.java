package edu.vanderbilt.cs.wutkam.scheml.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;

/** Reads a line from System.in
 */
public class Input extends BuiltinFunctionExpr {
    public Input(String name) {
        super(name, "void -> string");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        return new StringExpr(SchemlRuntime.getRepl().readLine());
    }
}

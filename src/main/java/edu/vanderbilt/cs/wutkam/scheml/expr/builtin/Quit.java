package edu.vanderbilt.cs.wutkam.scheml.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.VoidExpr;

/** Terminates the repl
 */
public class Quit extends BuiltinFunctionExpr {
    public Quit(String name) {
        super(name, "void -> void");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        System.exit(0);
        return new VoidExpr();
    }
}

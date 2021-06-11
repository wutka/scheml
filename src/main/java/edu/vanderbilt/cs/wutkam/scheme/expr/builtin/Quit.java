package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.VoidExpr;

/** Prints a string to stdout
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

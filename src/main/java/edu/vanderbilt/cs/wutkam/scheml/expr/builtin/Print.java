package edu.vanderbilt.cs.wutkam.scheml.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.VoidExpr;

/** Prints a string to stdout
 */
public class Print extends BuiltinFunctionExpr {
    public Print(String name) {
        super(name, "string -> void");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        Expression expr = args[0];
        // Type unification will ensure the expr is a string
        System.out.print(((StringExpr)expr).value);
        System.out.flush();
        return new VoidExpr();
    }
}

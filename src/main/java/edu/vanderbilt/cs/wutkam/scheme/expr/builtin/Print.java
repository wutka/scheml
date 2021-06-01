package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.ConsExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.VoidExpr;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/26/21
 * Time: 1:50 PM
 */
public class Print extends BuiltinFunctionExpr {
    public Print(String name) {
        super(name, "string -> void");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        Expression expr = args[0];
        if (expr instanceof StringExpr) {
            System.out.print(((StringExpr)expr).value);
        } else {
            System.out.print(expr.toString());
        }
        System.out.flush();
        return new VoidExpr();
    }
}

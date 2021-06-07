package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.ConsExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.IntExpr;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/26/21
 * Time: 1:50 PM
 */
public class Range extends BuiltinFunctionExpr {
    public Range(String name) {
        super(name, "int -> int -> cons int");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        ConsExpr curr = new ConsExpr();
        for (int i=((IntExpr)args[1]).value; i >= ((IntExpr)args[0]).value; i--) {
            curr = new ConsExpr(new IntExpr(i), curr);
        }
        return curr;
    }
}

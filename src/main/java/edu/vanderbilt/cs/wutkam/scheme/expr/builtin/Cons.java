package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.CharExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.ConsExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.StringExpr;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/26/21
 * Time: 1:50 PM
 */
public class Cons extends BuiltinFunctionExpr {
    public Cons(String name) {
        super(name, 2);
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        if (!(args[1] instanceof ConsExpr)) {
            throw new LispException("Second argument to "+name+" must be a list");
        }
        return new ConsExpr(args[0], (ConsExpr) args[1]);
    }
}

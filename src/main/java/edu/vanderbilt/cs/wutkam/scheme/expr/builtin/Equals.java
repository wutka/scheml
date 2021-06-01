package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.BoolExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.ConsExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/26/21
 * Time: 1:50 PM
 */
public class Equals extends BuiltinFunctionExpr {
    public Equals(String name) {
        super(name, "'a -> 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        return new BoolExpr(args[0].equals(args[1]));
    }
}

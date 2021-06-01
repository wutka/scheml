package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.ConsExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/26/21
 * Time: 1:50 PM
 */
public class Head extends BuiltinFunctionExpr {
    public Head(String name) {
        super(name, "cons 'a -> 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        if (!(args[0] instanceof ConsExpr)) {
            throw new LispException("First argument to "+name+" must be a list");
        }
        ConsExpr cons = (ConsExpr) args[0];
        if (cons.head == null) {
            throw new LispException("Tried to get head of empty list");
        }
        return cons.head;
    }
}

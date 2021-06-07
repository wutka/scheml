package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.ConsExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;

/** Returns the head of a list
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

        // Following the Scheme convention where (car '()) is an error
        // rather than the Lisp convention where (car nil) is nil
        if (cons.head == null) {
            throw new LispException("Tried to get head of empty list");
        }
        return cons.head;
    }
}

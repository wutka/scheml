package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;

/** Returns the head of a list
 */
public class Head extends BuiltinFunctionExpr {
    public Head(String name) {
        super(name, "cons 'a -> 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // Don't need to check the type
        AbstractTypeExpr cons = (AbstractTypeExpr) args[0];

        // Following the Scheme convention where (car '()) is an error
        // rather than the Lisp convention where (car nil) is nil
        if (cons.constructorName.equals("Nil")) {
            throw new LispException("Tried to get head of empty list");
        }
        return cons.values.get(0);
    }
}

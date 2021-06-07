package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.ConsExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;

/** Returns the tail of a list
 */
public class Tail extends BuiltinFunctionExpr {
    public Tail(String name) {
        super(name, "cons 'a -> cons 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // Type unification ensures this is a cons
        ConsExpr cons = (ConsExpr) args[0];
        if (cons.head == null) {
            return cons;
        } if (cons.tail == null) {
            return new ConsExpr();
        } else {
            return cons.tail;
        }
    }
}

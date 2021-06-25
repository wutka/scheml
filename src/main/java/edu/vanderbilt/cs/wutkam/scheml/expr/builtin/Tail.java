package edu.vanderbilt.cs.wutkam.scheml.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;

/** Returns the tail of a list
 */
public class Tail extends BuiltinFunctionExpr {
    public Tail(String name) {
        super(name, "cons 'a -> cons 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // Type unification ensures this is a cons
        AbstractTypeExpr cons = (AbstractTypeExpr) args[0];
        if (cons.constructorName.equals("Nil")) {
            return cons;
        } else {
            return cons.values.get(1);
        }
    }
}

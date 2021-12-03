package edu.vanderbilt.cs.wutkam.scheml.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;

/** A function to return the second item in a pair
 */
public class Snd extends BuiltinFunctionExpr {
    public Snd(String name) {
        super(name, "pair 'a 'b -> 'b");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // We don't don't check the types here, the type unification will take
        // care of that because of the function signature
        AbstractTypeExpr pairExpr = (AbstractTypeExpr) args[0];
        return pairExpr.values.get(1);
    }
}

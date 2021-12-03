package edu.vanderbilt.cs.wutkam.scheml.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;

/** A function to return the first item in a pair
 */
public class Fst extends BuiltinFunctionExpr {
    public Fst(String name) {
        super(name, "pair 'a 'b -> 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // We don't don't check the types here, the type unification will take
        // care of that because of the function signature
        AbstractTypeExpr pairExpr = (AbstractTypeExpr) args[0];
        return pairExpr.values.get(0);
    }
}

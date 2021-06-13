package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.IntExpr;

/** A function to find the length of a string
 */
public class Length extends BuiltinFunctionExpr {
    public Length(String name) {
        super(name, "cons 'a -> 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // We don't don't check the types here, the type unification will take
        // care of that because of the function signature
        AbstractTypeExpr curr = (AbstractTypeExpr) args[0];
        long len = 0;
        while (!curr.constructorName.equals("Nil")) {
            curr = (AbstractTypeExpr) curr.values.get(1);
            len++;
        }

        return new IntExpr(len);
    }
}

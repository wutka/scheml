package edu.vanderbilt.cs.wutkam.scheme.expr.builtin.list;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.IntExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.builtin.FailException;

/** Returns the nth element of a list
 */
public class Nth extends BuiltinFunctionExpr {
    public Nth(String name) {
        super(name, "int -> cons 'a -> 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // We don't don't check the types here, the type unification will take
        // care of that because of the function signature
        long n = ((IntExpr) args[0]).value;
        AbstractTypeExpr curr = (AbstractTypeExpr) args[1];
        while (n > 0) {
            if (curr.constructorName.equals("Nil")) {
                throw new FailException("Ran out of elements looking for nth element");
            }
            curr = (AbstractTypeExpr) curr.values.get(1);
            n--;
        }
        if (curr.constructorName.equals("Nil")) {
            throw new FailException("Ran out of elements looking for nth element");
        }

        return curr.values.get(0);
    }
}

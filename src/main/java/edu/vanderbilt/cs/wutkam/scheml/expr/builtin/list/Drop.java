package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.list;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.IntExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;

/** Drop the first n elements from a list
 */
public class Drop extends BuiltinFunctionExpr {
    public Drop(String name) {
        super(name, "int -> cons 'a -> cons 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // We don't don't check the types here, the type unification will take
        // care of that because of the function signature
        long n = ((IntExpr) args[0]).value;
        AbstractTypeExpr curr = (AbstractTypeExpr) args[1];
        while (n > 0) {
            if (curr.constructorName.equals("Nil")) {
                return curr;
            }
            curr = (AbstractTypeExpr) curr.values.get(1);
            n--;
        }
        return curr;
    }
}

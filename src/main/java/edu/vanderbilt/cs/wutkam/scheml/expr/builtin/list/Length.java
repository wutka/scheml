package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.list;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.IntExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;

/** A function to find the length of a string
 */
public class Length extends BuiltinFunctionExpr {
    public Length(String name) {
        super(name, "cons 'a -> int");
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

package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.list;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.BoolExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;

/** Given an item and a list, return true if the item is a member of the list
 */
public class Member extends BuiltinFunctionExpr {
    public Member(String name) {
        super(name, "'a -> cons 'a -> bool");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // We don't don't check the types here, the type unification will take
        // care of that because of the function signature
        AbstractTypeExpr curr = (AbstractTypeExpr) args[1];
        while (!curr.constructorName.equals("Nil")) {
            if (args[0].equals(curr.values.get(0))) {
                return BoolExpr.TRUE;
            }
            curr = (AbstractTypeExpr) curr.values.get(1);
        }

        return BoolExpr.FALSE;
    }
}

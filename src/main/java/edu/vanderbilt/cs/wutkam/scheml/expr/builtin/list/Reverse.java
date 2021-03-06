package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.list;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;

/** Reverses a string
 */
public class Reverse extends BuiltinFunctionExpr {
    public Reverse(String name) {
        super(name, "cons 'a -> cons 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // We don't don't check the types here, the type unification will take
        // care of that because of the function signature
        AbstractTypeExpr curr = (AbstractTypeExpr) args[0];
        AbstractTypeExpr destCurr = ConsTypeDecl.newNil();
        while (!curr.constructorName.equals("Nil")) {
            destCurr = ConsTypeDecl.newCons(curr.values.get(0), destCurr);
            curr = (AbstractTypeExpr) curr.values.get(1);
        }

        return destCurr;
    }
}

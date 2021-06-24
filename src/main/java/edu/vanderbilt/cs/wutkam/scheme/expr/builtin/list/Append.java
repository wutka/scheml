package edu.vanderbilt.cs.wutkam.scheme.expr.builtin.list;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheme.type.builtin.ConsTypeDecl;

import java.util.Stack;

/** Given two lists appends the second list to the end of the first
 */
public class Append extends BuiltinFunctionExpr {
    public Append(String name) {
        super(name, "cons 'a -> cons 'a -> cons 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // We don't don't check the types here, the type unification will take
        // care of that because of the function signature

        AbstractTypeExpr curr = (AbstractTypeExpr) args[0];
        Stack<Expression> resultStack = new Stack<>();
        while (!curr.constructorName.equals("Nil")) {
            resultStack.push(curr.values.get(0));
            curr = (AbstractTypeExpr) curr.values.get(1);
        }

        AbstractTypeExpr destCurr = (AbstractTypeExpr) args[1];
        while (!resultStack.isEmpty()) {
            destCurr = ConsTypeDecl.newCons(resultStack.pop(), destCurr);
        }
        return destCurr;
    }
}

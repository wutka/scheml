package edu.vanderbilt.cs.wutkam.scheme.expr.builtin.list;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.IntExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.builtin.FailException;
import edu.vanderbilt.cs.wutkam.scheme.type.builtin.ConsTypeDecl;

import java.util.Stack;

/** Replaces the nth element of a list with a new value
 */
public class ReplaceNth extends BuiltinFunctionExpr {
    public ReplaceNth(String name) {
        super(name, "int -> 'a -> cons 'a -> cons 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // We don't don't check the types here, the type unification will take
        // care of that because of the function signature
        long n = ((IntExpr) args[0]).value;
        AbstractTypeExpr curr = (AbstractTypeExpr) args[2];
        Stack<Expression> resultStack = new Stack<>();
        while (n > 0) {
            if (curr.constructorName.equals("Nil")) {
                throw new FailException("Ran out of elements looking for nth element");
            }
            resultStack.push(curr.values.get(0));
            curr = (AbstractTypeExpr) curr.values.get(1);
            n--;
        }
        if (curr.constructorName.equals("Nil")) {
            throw new FailException("Ran out of elements looking for nth element");
        }

        curr = ConsTypeDecl.newCons(args[1], curr.values.get(1));

        while (!resultStack.isEmpty()) {
            curr = ConsTypeDecl.newCons(resultStack.pop(), curr);
        }
        return curr;
    }
}

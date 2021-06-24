package edu.vanderbilt.cs.wutkam.scheme.expr.builtin.list;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheme.type.builtin.ConsTypeDecl;

import java.util.Stack;

/** Given an item and a list, removes the first occurrence of the item from the list
 */
public class Remove extends BuiltinFunctionExpr {
    public Remove(String name) {
        super(name, "'a -> cons 'a -> cons 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // We don't don't check the types here, the type unification will take
        // care of that because of the function signature
        AbstractTypeExpr curr = (AbstractTypeExpr) args[1];
        Stack<Expression> resultStack = new Stack<>();
        while (!curr.constructorName.equals("Nil")) {
            if (args[0].equals(curr.values.get(0))) {
                curr = (AbstractTypeExpr) curr.values.get(1);
                while (!resultStack.isEmpty()) {
                    curr = ConsTypeDecl.newCons(resultStack.pop(), curr);
                }
                return curr;
            } else {
                resultStack.push(curr.values.get(0));
            }
            curr = (AbstractTypeExpr) curr.values.get(1);
        }

        return args[1];
    }
}

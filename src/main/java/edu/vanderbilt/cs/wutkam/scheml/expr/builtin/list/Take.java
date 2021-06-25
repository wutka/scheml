package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.list;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.IntExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;

import java.util.Stack;

/** Takes the first n elements from a list and returns a new list of those elements
 */
public class Take extends BuiltinFunctionExpr {
    public Take(String name) {
        super(name, "int -> cons 'a -> cons 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // We don't don't check the types here, the type unification will take
        // care of that because of the function signature
        long n = ((IntExpr) args[0]).value;
        AbstractTypeExpr curr = (AbstractTypeExpr) args[1];
        Stack<Expression> expressionStack = new Stack<>();
        while (n > 0) {
            if (curr.constructorName.equals("Nil")) {
                break;
            }
            expressionStack.push(curr.values.get(0));
            curr = (AbstractTypeExpr) curr.values.get(1);
            n--;
        }

        curr = ConsTypeDecl.newNil();
        while (!expressionStack.isEmpty()) {
            curr = ConsTypeDecl.newCons(expressionStack.pop(), curr);
        }

        return curr;
    }
}

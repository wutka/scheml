package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.list;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Applicable;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

/** Applies a function to every element of a list and returns a list of the results
 */
public class Map extends BuiltinFunctionExpr {
    public Map(String name) {
        super(name, "('a -> 'b) -> cons 'a -> cons 'b");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // We don't don't check the types here, the type unification will take
        // care of that because of the function signature
        Applicable f = (Applicable) args[0];

        AbstractTypeExpr curr = (AbstractTypeExpr) args[1];
        Stack<Expression> resultStack = new Stack<>();
        while (!curr.constructorName.equals("Nil")) {
            List<Expression> argList = new ArrayList<>();
            argList.add(curr.values.get(0));
            resultStack.push(applyLoop(f, argList));
            curr = (AbstractTypeExpr) curr.values.get(1);
        }

        AbstractTypeExpr destCurr = ConsTypeDecl.newNil();
        while (!resultStack.isEmpty()) {
            destCurr = ConsTypeDecl.newCons(resultStack.pop(), destCurr);
        }
        return destCurr;
    }
}

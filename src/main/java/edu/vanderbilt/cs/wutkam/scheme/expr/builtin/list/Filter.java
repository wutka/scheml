package edu.vanderbilt.cs.wutkam.scheme.expr.builtin.list;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Applicable;
import edu.vanderbilt.cs.wutkam.scheme.expr.BoolExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheme.type.builtin.ConsTypeDecl;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

/** Given a predicate function and a list of items, returns the list of items for which
 * the predicate returns true.
 */
public class Filter extends BuiltinFunctionExpr {
    public Filter(String name) {
        super(name, "('a -> bool) -> cons 'a -> cons 'a");
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
            Expression result = applyLoop(f, argList);
            if (result.equals(BoolExpr.TRUE)) {
                resultStack.push(curr.values.get(0));
            }
            curr = (AbstractTypeExpr) curr.values.get(1);
        }

        AbstractTypeExpr destCurr = ConsTypeDecl.newNil();
        while (!resultStack.isEmpty()) {
            destCurr = ConsTypeDecl.newCons(resultStack.pop(), destCurr);
        }
        return destCurr;
    }
}

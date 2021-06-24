package edu.vanderbilt.cs.wutkam.scheme.expr.builtin.list;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Applicable;
import edu.vanderbilt.cs.wutkam.scheme.expr.BoolExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.builtin.BuiltinFunctionExpr;

import java.util.ArrayList;
import java.util.List;

/** Applies a predicate function (a -> bool) to each element of a list of type a
 * and returns false only if the predicate is false for each element
 */
public class Some extends BuiltinFunctionExpr {
    public Some(String name) {
        super(name, "('a -> bool) -> cons 'a -> bool");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // We don't don't check the types here, the type unification will take
        // care of that because of the function signature
        Applicable f = (Applicable) args[0];

        AbstractTypeExpr curr = (AbstractTypeExpr) args[1];
        while (!curr.constructorName.equals("Nil")) {
            List<Expression> argList = new ArrayList<>();
            argList.add(curr.values.get(0));
            if (applyLoop(f, argList).equals(BoolExpr.TRUE)) {
                return BoolExpr.TRUE;
            }
            curr = (AbstractTypeExpr) curr.values.get(1);
        }

        return BoolExpr.FALSE;
    }
}

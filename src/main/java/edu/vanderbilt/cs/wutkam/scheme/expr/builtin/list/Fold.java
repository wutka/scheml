package edu.vanderbilt.cs.wutkam.scheme.expr.builtin.list;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Applicable;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.builtin.BuiltinFunctionExpr;

import java.util.ArrayList;
import java.util.List;

/** Given a function from a -> b -> b, and a list of a, and a starter param of type b,
 * apply the function to each element of the list, using the result from the previous
 * application as the second parameter to the function. When the list has been processed,
 * return the last result from the function.
 * For example, (fold * 1 (range 1 6)) will multiply the numbers between 1 and 6
 * together this way:
 *   (* 1 1) = 1
 *   (* 2 1) = 2
 *   (* 3 2) = 6
 *   (* 4 6) = 24
 *   (* 5 24) = 120
 *   (* 6 120) = 720
 *   return 720
 */
public class Fold extends BuiltinFunctionExpr {
    public Fold(String name) {
        super(name, "('a -> 'b -> 'b) -> 'b -> cons 'a -> 'b");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // We don't don't check the types here, the type unification will take
        // care of that because of the function signature
        Applicable f = (Applicable) args[0];

        Expression result = args[1];
        AbstractTypeExpr curr = (AbstractTypeExpr) args[2];
        while (!curr.constructorName.equals("Nil")) {
            List<Expression> argList = new ArrayList<>();
            argList.add(curr.values.get(0));
            argList.add(result);
            result = applyLoop(f, argList);
            curr = (AbstractTypeExpr) curr.values.get(1);
        }

        return result;
    }
}
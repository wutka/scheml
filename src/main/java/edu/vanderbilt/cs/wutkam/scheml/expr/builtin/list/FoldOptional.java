package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.list;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Applicable;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;

import java.util.ArrayList;
import java.util.List;

/** Given a function from b -> a -> b, and a list of a, and a starter param of type b,
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
public class FoldOptional extends BuiltinFunctionExpr {
    public FoldOptional(String name) {
        super(name, "('b -> 'a -> option 'b) -> 'b -> cons 'a -> 'b");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // We don't check the types here, the type unification will take
        // care of that because of the function signature
        Applicable f = (Applicable) args[0];

        Expression result = args[1];
        AbstractTypeExpr curr = (AbstractTypeExpr) args[2];
        while (!curr.constructorName.equals("Nil")) {
            List<Expression> argList = new ArrayList<>();
            argList.add(curr.values.get(0));
            argList.add(result);
            AbstractTypeExpr tempResult = (AbstractTypeExpr) applyLoop(f, argList);
            if (!tempResult.constructorName.equals("Nothing")) {
                result = tempResult.values.get(0);
            }
            curr = (AbstractTypeExpr) curr.values.get(1);
        }

        return result;
    }
}
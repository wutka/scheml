package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.array;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Applicable;
import edu.vanderbilt.cs.wutkam.scheml.expr.ArrayExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;

import java.util.ArrayList;
import java.util.List;

/** Given a function from b -> a -> b, and an array of a, and a starter param of type b,
 * apply the function to each element of the array, using the result from the previous
 * application as the second parameter to the function. When the array has been processed,
 * return the last result from the function.
 * For example, (fold * 1 (make-array (range 1 6))) will multiply the numbers between 1 and 6
 * together this way:
 *   (* 1 1) = 1
 *   (* 2 1) = 2
 *   (* 3 2) = 6
 *   (* 4 6) = 24
 *   (* 5 24) = 120
 *   (* 6 120) = 720
 *   return 720
 */
public class ArrayFold extends BuiltinFunctionExpr {
    public ArrayFold(String name) {
        super(name, "('b -> 'a -> 'b) -> 'b -> array 'a -> 'b");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // We don't don't check the types here, the type unification will take
        // care of that because of the function signature
        Applicable f = (Applicable) args[0];

        Expression result = args[1];
        ArrayExpr arr = (ArrayExpr) args[2];
        for (int i=0; i < arr.values.length; i++) {
            List<Expression> argList = new ArrayList<>();
            argList.add(result);
            argList.add(arr.values[i]);
            result = applyLoop(f, argList);
        }

        return result;
    }
}
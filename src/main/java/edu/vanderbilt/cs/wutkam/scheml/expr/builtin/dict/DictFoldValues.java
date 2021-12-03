package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.dict;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Applicable;
import edu.vanderbilt.cs.wutkam.scheml.expr.DictExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;

import java.util.ArrayList;
import java.util.List;

/** Given a function from b -> c -> c, and a dict of a b, and a starter param of type c,
 * apply the function to each value of the dict, using the result from the previous
 * application as the second parameter to the function. When the dict has been processed,
 * return the last result from the function.
 */
public class DictFoldValues extends BuiltinFunctionExpr {
    public DictFoldValues(String name) {
        super(name, "('b -> 'c -> 'c) -> 'c -> dict 'a 'b -> 'c");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // We don't don't check the types here, the type unification will take
        // care of that because of the function signature
        Applicable f = (Applicable) args[0];

        Expression result = args[1];
        DictExpr dict = (DictExpr) args[2];
        for (Expression value: dict.dict.values()) {
            List<Expression> argList = new ArrayList<>();
            argList.add(value);
            argList.add(result);
            result = applyLoop(f, argList);
        }

        return result;
    }
}
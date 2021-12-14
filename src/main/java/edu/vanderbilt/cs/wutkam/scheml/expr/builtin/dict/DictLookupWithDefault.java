package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.dict;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.DictExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;

/** Finds a value in a dict
 */
public class DictLookupWithDefault extends BuiltinFunctionExpr {
    public DictLookupWithDefault(String name) {
        super(name, "dict 'a 'b -> 'a -> 'b -> 'b");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        DictExpr dict = (DictExpr) args[0];
        Expression key = args[1];
        Expression result = dict.dict.get(key);
        if (result == null) result = args[2];
        return result;
    }
}
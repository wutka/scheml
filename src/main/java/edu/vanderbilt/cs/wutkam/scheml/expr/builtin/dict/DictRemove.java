package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.dict;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.DictExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;

/** Removes a key from a dict
 */
public class DictRemove extends BuiltinFunctionExpr {
    public DictRemove(String name) {
        super(name, "dict 'a 'b -> 'a -> dict 'a 'b");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        DictExpr dict = (DictExpr) args[0];
        Expression key = args[1];
        dict.dict.remove(key);
        return dict;
    }
}
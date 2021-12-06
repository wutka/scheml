package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.dict;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.DictExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;

/** Puts a key/value into a dict and returns the value
 */
public class DictPut extends BuiltinFunctionExpr {
    public DictPut(String name) {
        super(name, "dict 'a 'b -> 'a -> 'b -> dict 'a 'b");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        DictExpr dict = (DictExpr) args[0];
        Expression key = args[1];
        Expression value = args[2];
        dict.dict.put(key, value);
        return dict;
    }
}
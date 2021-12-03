package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.dict;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Applicable;
import edu.vanderbilt.cs.wutkam.scheml.expr.DictExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.IntExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/** Returns the number of entries in the dict
 */
public class DictLen extends BuiltinFunctionExpr {
    public DictLen(String name) {
        super(name, "dict 'a 'b -> int");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        DictExpr dict = (DictExpr) args[0];
        return new IntExpr(dict.dict.size());
    }
}
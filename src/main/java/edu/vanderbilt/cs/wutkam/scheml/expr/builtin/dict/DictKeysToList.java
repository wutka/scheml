package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.dict;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.DictExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;

import java.util.Arrays;
import java.util.Map;

/** Converts a dict to a list of keys */
public class DictKeysToList extends BuiltinFunctionExpr {
    public DictKeysToList(String name) {
        super(name, "dict 'a 'b -> cons 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        DictExpr dict = (DictExpr) args[0];
        AbstractTypeExpr curr = ConsTypeDecl.newNil();
        for (Expression key: dict.dict.keySet()) {
             curr = ConsTypeDecl.newCons(key, curr);
        }
        return curr;
    }
}

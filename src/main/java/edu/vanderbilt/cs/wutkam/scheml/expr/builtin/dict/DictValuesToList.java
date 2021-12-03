package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.dict;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.DictExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;

/** Converts a dict to a list of values */
public class DictValuesToList extends BuiltinFunctionExpr {
    public DictValuesToList(String name) {
        super(name, "dict 'a 'b -> cons 'b");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        DictExpr dict = (DictExpr) args[0];
        AbstractTypeExpr curr = ConsTypeDecl.newNil();
        for (Expression value: dict.dict.values()) {
             curr = ConsTypeDecl.newCons(value, curr);
        }
        return curr;
    }
}

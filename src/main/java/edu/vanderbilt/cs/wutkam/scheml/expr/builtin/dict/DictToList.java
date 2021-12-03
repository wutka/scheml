package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.dict;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.DictExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;

import java.util.Arrays;
import java.util.Map;

/** Converts a dict to a list of key-value pairs */
public class DictToList extends BuiltinFunctionExpr {
    public DictToList(String name) {
        super(name, "dict 'a 'b -> cons (pair 'a 'b)");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        DictExpr dict = (DictExpr) args[0];
        AbstractTypeExpr curr = ConsTypeDecl.newNil();
        for (Map.Entry<Expression,Expression> entry: dict.dict.entrySet()) {
            AbstractTypeExpr pairExpr = new AbstractTypeExpr("pair", "Pair",
                    Arrays.asList(entry.getKey(), entry.getValue()));
             curr = ConsTypeDecl.newCons(pairExpr, curr);
        }
        return curr;
    }
}

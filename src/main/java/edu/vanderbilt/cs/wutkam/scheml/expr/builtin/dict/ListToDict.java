package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.dict;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.ArrayExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.DictExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/** Converts a list to a dict */
public class ListToDict extends BuiltinFunctionExpr {
    public ListToDict(String name) {
        super(name, "cons (pair 'a 'b) -> dict 'a 'b");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        AbstractTypeExpr curr = (AbstractTypeExpr) args[0];
        Map<Expression,Expression> newDict = new HashMap<>();

        while (curr.constructorName.equals("Cons")) {
            AbstractTypeExpr pairExpr = (AbstractTypeExpr) curr.values.get(0);
            newDict.put(pairExpr.values.get(0), pairExpr.values.get(1));
            curr = (AbstractTypeExpr) curr.values.get(1);
        }
        return new DictExpr(newDict);
    }
}

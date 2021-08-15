package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.array;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.ArrayExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;

import java.util.ArrayList;
import java.util.List;

/** Converts a list to an array */
public class ListToArray extends BuiltinFunctionExpr {
    public ListToArray(String name) {
        super(name, "cons 'a -> array 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        AbstractTypeExpr curr = (AbstractTypeExpr) args[0];
        List<Expression> values = new ArrayList<>();

        while (curr.constructorName.equals("Cons")) {
            values.add(curr.values.get(0));
            curr = (AbstractTypeExpr) curr.values.get(1);
        }
        return new ArrayExpr(new ListExpr(values));
    }
}

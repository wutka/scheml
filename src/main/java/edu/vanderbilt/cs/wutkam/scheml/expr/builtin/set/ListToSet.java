package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.set;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.SetExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;

import java.util.HashSet;
import java.util.Set;

/** Converts a list to a set */
public class ListToSet extends BuiltinFunctionExpr {
    public ListToSet(String name) {
        super(name, "cons 'a -> set 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        AbstractTypeExpr curr = (AbstractTypeExpr) args[0];
        Set<Expression> set = new HashSet<>();

        while (curr.constructorName.equals("Cons")) {
            set.add(curr.values.get(0));
            curr = (AbstractTypeExpr) curr.values.get(1);
        }
        return new SetExpr(set);
    }
}

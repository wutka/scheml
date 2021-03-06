package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.sexpr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.FailException;

public class SexprToList extends BuiltinFunctionExpr {
    public SexprToList(String name) {
        super(name, "sexpr -> cons sexpr");
    }

    @Override
    protected Expression executeBuiltin(Expression[] args) throws LispException {
        AbstractTypeExpr curr = (AbstractTypeExpr) args[0];
        if (curr.constructorName.equals("SexprList")) {
            return curr.values.get(0);
        } else {
            throw new FailException("S-expression is not a SexprList");
        }
    }
}

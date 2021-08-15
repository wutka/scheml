package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.sexpr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.BoolExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.FailException;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.SexprTypeDecl;

public class SexprToDouble extends BuiltinFunctionExpr {
    public SexprToDouble(String name) {
        super(name, "sexpr -> double");
    }

    @Override
    protected Expression executeBuiltin(Expression[] args) throws LispException {
        AbstractTypeExpr curr = (AbstractTypeExpr) args[0];
        if (curr.constructorName.equals("SexprDouble")) {
            return curr.values.get(0);
        } else {
            throw new FailException("S-expression is not a SexprDouble");
        }
    }
}

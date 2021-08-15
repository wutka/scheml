package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.sexpr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.BoolExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.FailException;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.SexprTypeDecl;

public class SexprToBool extends BuiltinFunctionExpr {
    public SexprToBool(String name) {
        super(name, "sexpr -> bool");
    }

    @Override
    protected Expression executeBuiltin(Expression[] args) throws LispException {
        AbstractTypeExpr curr = (AbstractTypeExpr) args[0];
        if (curr.constructorName.equals("SexprBool")) {
            return curr.values.get(0);
        } else {
            throw new FailException("S-expression is not a SexprBool");
        }
    }
}

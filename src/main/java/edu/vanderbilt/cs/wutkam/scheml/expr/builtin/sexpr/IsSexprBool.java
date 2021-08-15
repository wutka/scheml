package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.sexpr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.BoolExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.SexprTypeDecl;

import java.util.Stack;

public class IsSexprBool extends BuiltinFunctionExpr {
    public IsSexprBool(String name) {
        super(name, "sexpr -> bool");
    }

    @Override
    protected Expression executeBuiltin(Expression[] args) throws LispException {
        AbstractTypeExpr curr = (AbstractTypeExpr) args[0];
        return new BoolExpr(curr.constructorName.equals("SexprBool"));
    }
}

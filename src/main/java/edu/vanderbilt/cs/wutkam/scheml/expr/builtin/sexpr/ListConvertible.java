package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.sexpr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.BoolExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.SexprTypeDecl;

public class ListConvertible extends BuiltinFunctionExpr {
    public ListConvertible(String name) {
        super(name, "sexpr -> bool");
    }
    @Override
    protected Expression executeBuiltin(Expression[] args) throws LispException {
        if (!SexprTypeDecl.isSexpr(args[0])) return BoolExpr.FALSE;

        Expression sexprValue = ((AbstractTypeExpr)args[0]).values.get(0);

        if (!(sexprValue instanceof AbstractTypeExpr)) return BoolExpr.FALSE;


        AbstractTypeExpr curr = (AbstractTypeExpr) sexprValue;
        if (!(curr.typeName.equals(ConsTypeDecl.consTypeName))) return BoolExpr.FALSE;

        String valueConstructor = null;
        while (curr.constructorName.equals("Cons")) {
            Expression currExpr = curr.values.get(0);
            if (!SexprTypeDecl.isSexpr(currExpr)) return BoolExpr.FALSE;

            if (valueConstructor == null) {
                valueConstructor = ((AbstractTypeExpr)currExpr).constructorName;
            } else {
                if (!valueConstructor.equals(((AbstractTypeExpr)curr.values.get(0)).constructorName)) {
                    return BoolExpr.FALSE;
                }
            }
            curr = (AbstractTypeExpr) curr.values.get(1);
        }
        return BoolExpr.TRUE;
    }
}

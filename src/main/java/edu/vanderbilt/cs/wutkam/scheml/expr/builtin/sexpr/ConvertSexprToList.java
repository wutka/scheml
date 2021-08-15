package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.sexpr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.BoolExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.FailException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.SexprTypeDecl;

import java.util.Stack;

public class ConvertSexprToList extends BuiltinFunctionExpr {
    public ConvertSexprToList(String name) {
        super(name, "sexpr -> cons 'a");
    }

    @Override
    protected Expression executeBuiltin(Expression[] args) throws LispException {
        Expression sexprValue = ((AbstractTypeExpr)args[0]).values.get(0);

        if (!(sexprValue instanceof AbstractTypeExpr)) {
            throw new FailException("S-expression is not a list");
        }

        AbstractTypeExpr curr = (AbstractTypeExpr) sexprValue;
        if (!(curr.typeName.equals(ConsTypeDecl.consTypeName))) return BoolExpr.FALSE;

        String valueConstructor = null;
        Stack<Expression> itemStack = new Stack<>();

        while (curr.constructorName.equals("Cons")) {
            Expression currExpr = curr.values.get(0);
            AbstractTypeExpr abstractTypeExpr = (AbstractTypeExpr) currExpr;

            if (valueConstructor == null) {
                valueConstructor = ((AbstractTypeExpr)currExpr).constructorName;
                itemStack.push(abstractTypeExpr.values.get(0));
            } else {
                if (!valueConstructor.equals(((AbstractTypeExpr)curr.values.get(0)).constructorName)) {
                    throw new FailException("S-expression list contains mixed types");
                }
                itemStack.push(abstractTypeExpr.values.get(0));
            }
            curr = (AbstractTypeExpr) curr.values.get(1);
        }

        curr = ConsTypeDecl.newNil();
        while (!itemStack.isEmpty()) {
            Expression expr = itemStack.pop();
            curr = ConsTypeDecl.newCons(expr, curr);
        }
        return curr;
    }
}

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

public class SexprToList extends BuiltinFunctionExpr {
    public SexprToList(String name) {
        super(name, "sexpr -> cons 'a");
    }
    @Override
    protected Expression executeBuiltin(Expression[] args) throws LispException {
        if (!SexprTypeDecl.isSexpr(args[0])) {
            throw new FailException("Expression is not an S-expression");
        }

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
        TypeRef listType = null;
        while (!itemStack.isEmpty()) {
            Expression expr = itemStack.pop();
            if (listType == null) {
                listType = new TypeRef();
                expr.unify(listType, new Environment<>());
            }
            curr = ConsTypeDecl.newCons(expr, curr);
        }
        return curr;
    }
}

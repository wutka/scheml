package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.forms.FormExpander;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.AbstractType;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.SexprTypeDecl;

import java.util.ArrayList;
import java.util.List;

public class EvalExpr implements Expression {
    Expression expr;
    boolean isTopLevel;

    public EvalExpr(Expression expr, boolean isTopLevel) {
        this.expr = expr;
        this.isTopLevel = isTopLevel;
    }

    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        Expression evalMe = unliteralizeSymbols(
                SexprTypeDecl.toExpression(
                        (AbstractTypeExpr) expr.evaluate(env, false), true));

        if (evalMe instanceof SymbolExpr) {
            evalMe = evalMe.evaluate(env, inTailPosition);
        }
        if (evalMe instanceof ListExpr) {
            evalMe = FormExpander.expand((ListExpr) evalMe, isTopLevel);
        }
        return SexprTypeDecl.fromExpression(evalMe.evaluate(env, false).toScheml(), env);
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        expr.unify(new TypeRef(new AbstractType(SexprTypeDecl.sexprTypeName, new ArrayList<>())), env);
        typeRef.unify(new TypeRef(new AbstractType(SexprTypeDecl.sexprTypeName, new ArrayList<>())));
    }

    @Override
    public Expression toScheml() {
        List<Expression> scheml = new ArrayList<>();
        scheml.add(new SymbolLiteralExpr("eval"));
        scheml.add(expr.toScheml());
        return new ListExpr(scheml);
    }

    Expression unliteralizeSymbols(Expression expr) {
        if (expr instanceof SymbolLiteralExpr) {
            return new SymbolExpr(((SymbolLiteralExpr)expr).value);
        } else if (expr instanceof ListExpr) {
            List<Expression> exprs = new ArrayList<>();
            for (Expression e: ((ListExpr)expr).elementsFrom(0)) {
                exprs.add(unliteralizeSymbols(e));
            }
            return new ListExpr(exprs);
        } else {
            return expr;
        }
    }
}

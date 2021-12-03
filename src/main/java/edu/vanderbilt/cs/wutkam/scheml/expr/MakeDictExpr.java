package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.DictType;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheml.type.UnifyException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class MakeDictExpr implements Expression {
    List<Expression> initialValues;

    public MakeDictExpr(List<Expression> initialValues) throws LispException {
        this.initialValues = initialValues;
    }

    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        List<Expression> evaluated = new ArrayList<>();
        for (Expression expr: initialValues) {
            ListExpr pairExpr = (ListExpr) expr;
            Expression keyEvaluated = pairExpr.getElement(0).evaluate(env, false);
            Expression valueEvaluated = pairExpr.getElement(1).evaluate(env, false);
            evaluated.add(new ListExpr(Arrays.asList(keyEvaluated, valueEvaluated)));
        }
        return new DictExpr(new ListExpr(evaluated));
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        TypeRef initialKeyValuesType = new TypeRef();
        TypeRef initialValueValuesType = new TypeRef();
        for (Expression expr: initialValues) {
            try {
                ListExpr pairExpr = (ListExpr) expr;
                pairExpr.getElement(0).unify(initialKeyValuesType, env);
                pairExpr.getElement(1).unify(initialValueValuesType, env);
            } catch (UnifyException exc) {
                throw UnifyException.addCause("Unable to unify dict element types", exc);
            }
        }
        typeRef.unify(new TypeRef(new DictType(initialKeyValuesType, initialValueValuesType)));
    }

    @Override
    public Expression toScheml() {
        List<Expression> scheml = new ArrayList<>();
        scheml.add(new SymbolLiteralExpr("make-dict"));
        for (Expression expr: initialValues) {
            scheml.add(expr.toScheml());
        }
        return new ListExpr(scheml);
    }
}

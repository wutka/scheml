package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.FunctionType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheme.type.UnifyException;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class ListExpr implements Expression {
    public List<Expression> elements;

    public ListExpr() {
        elements = new ArrayList<>();
    }

    public ListExpr(List<Expression> elements) {
        this.elements = elements;
    }

    public Expression evaluate(Environment<Expression> env) throws LispException {
        if (elements.size() == 0) {
            throw new LispException("Cannot evaluate an empty list");
        }
        Expression targetExpression = elements.get(0);
        Expression maybeFunctionExpr = targetExpression.evaluate(env);

        if (!(maybeFunctionExpr instanceof FunctionExpr)) {
            throw new LispException("Expression "+maybeFunctionExpr.toString()+" is not a function");
        }
        FunctionExpr targetFunction = (FunctionExpr) targetExpression.evaluate(env);

        ArrayList<Expression> parameterList = new ArrayList<>();
        for (int i=1; i < elements.size(); i++) {
            parameterList.add(elements.get(i).evaluate(env));
        }
        return targetFunction.apply(parameterList, env);
    }

    @Override
    public void unify(TypeRef ref, Environment<TypeRef> env) throws LispException {
        if (elements.size() == 0) {
            throw new LispException("Cannot unify an empty list");
        }

        Expression targetExpression = elements.get(0);
        TypeRef targetType = new TypeRef();
        try {
            targetExpression.unify(targetType, env);
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Unable to unify application target", exc);
        }

        FunctionType targetFunc;
        if (!(targetType.getType() instanceof FunctionType)) {
            TypeRef[] paramTypes = new TypeRef[elements.size()-1];
            for (int i=0; i < paramTypes.length; i++) paramTypes[i] = new TypeRef();
            TypeRef returnType = new TypeRef();
            targetFunc = new FunctionType(paramTypes.length, paramTypes, returnType);
        } else {
            targetFunc = (FunctionType) targetType.getType();
        }


        List<Expression> parameters = elements.subList(1, elements.size());
        if (parameters.size() > targetFunc.arity) {
            throw new UnifyException("Too many parameters in apply, expected "+targetFunc.arity+", got "+
                    parameters.size());
        }

        TypeRef[] unifiedParams = new TypeRef[parameters.size()];
        for (int i=0; i < parameters.size(); i++) {
            unifiedParams[i] = new TypeRef();
            parameters.get(i).unify(unifiedParams[i], env);
        }

        for (int i=0; i < parameters.size(); i++) {
            unifiedParams[i].unify(targetFunc.paramTypes[i]);
        }

        ref.unify(targetFunc.returnType);
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append('(');
        boolean first = true;
        for (Expression elem: elements) {
            if (!first) builder.append(' ');
            first = false;
            builder.append(elem.toString());
        }
        builder.append(')');
        return builder.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ListExpr listExpr = (ListExpr) o;
        return Objects.equals(elements, listExpr.elements);
    }

    @Override
    public int hashCode() {
        return Objects.hash(elements);
    }
}

package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.FunctionType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheme.type.UnifyException;

import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/25/21
 * Time: 2:07 PM
 */
public class FunctionExpr implements Expression {
    public final int arity;
    public final List<Expression> partialArgs;
    public final List<Expression> targetExpressions;
    public final List<SymbolExpr> parameterList;

    public TypeRef[] paramTypes;
    public TypeRef returnType;

    public FunctionExpr(int arity, List<SymbolExpr> parameterList, List<Expression> targetExpressions) {
        this.arity = arity;
        this.parameterList = parameterList;
        this.targetExpressions = targetExpressions;
        this.partialArgs = new ArrayList<>();
    }

    public FunctionExpr(FunctionExpr partialFunc, List<Expression> partialArgs) {
        this.arity = partialFunc.arity;
        this.parameterList = partialFunc.parameterList;
        this.targetExpressions = partialFunc.targetExpressions;
        this.partialArgs = new ArrayList<>();
        this.partialArgs.addAll(partialFunc.partialArgs);
        this.partialArgs.addAll(partialArgs);
    }

    public void setType(TypeRef[] paramTypes, TypeRef returnType) {
        this.paramTypes = paramTypes;
        this.returnType = returnType;
    }

    @Override
    public Expression evaluate(Environment<Expression> env) throws LispException {
        return Expression.super.evaluate(env);
    }

    protected Expression apply(List<Expression> arguments, Environment<Expression> env)
        throws LispException {
        if (arguments.size() + partialArgs.size() > arity) {
            throw new LispException("Too many parameters passed to function "+this);
        }

        if (arguments.size() + partialArgs.size() == arity) {
            Environment<Expression> funcEnv = new Environment<>(env);
            for (int i=0; i < arity; i++) {
                if (i < partialArgs.size()) {
                    funcEnv.define(parameterList.get(i).value, partialArgs.get(i));
                } else {
                    funcEnv.define(parameterList.get(i).value, arguments.get(i-partialArgs.size()));
                }
            }
            Expression last = null;
            for (Expression target: targetExpressions) {
                last = target.evaluate(funcEnv);
            }
            return last;
        } else {
            return new FunctionExpr(this, arguments);
        }
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        TypeRef[] paramTypeRefs = new TypeRef[arity];
        for (int i = 0; i < paramTypeRefs.length; i++) paramTypeRefs[i] = new TypeRef();

        for (int i = 0; i < partialArgs.size(); i++) {
            try {
                partialArgs.get(i).unify(paramTypeRefs[i], env);
            } catch (UnifyException exc) {
                throw UnifyException.addCause("Cannot unify function parameter " + paramTypeRefs[i].getType(), exc);
            }
        }

        Environment<TypeRef> funcEnv = new Environment<>(env);
        for (int i = 0; i < arity; i++) {
            String paramName = parameterList.get(i).value;
            funcEnv.define(paramName, paramTypeRefs[i]);
        }

        TypeRef returnType = new TypeRef();
        TypeRef lastExpr = new TypeRef();
        for (Expression expr: targetExpressions) {
            lastExpr = new TypeRef();
            try {
                expr.unify(lastExpr, funcEnv);
            } catch (UnifyException exc) {
                throw UnifyException.addCause("Can't unify function body", exc);
            }
        }
        try {
            returnType.unify(lastExpr);
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Can't unify function return type", exc);
        }

        FunctionType thisType = new FunctionType(arity, paramTypeRefs, returnType);
        try {
            typeRef.unify(new TypeRef(thisType));
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Can't unify function with "+typeRef.getType(), exc);
        }
    }
}

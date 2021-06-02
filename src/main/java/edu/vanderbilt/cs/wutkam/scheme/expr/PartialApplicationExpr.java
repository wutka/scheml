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
 * Date: 6/2/21
 * Time: 11:19 AM
 */
public class PartialApplicationExpr implements Expression, Applicable {
    FunctionExpr targetFunc;
    List<Expression> partialArgs;

    public PartialApplicationExpr(FunctionExpr targetFunction, List<Expression> args) {
        this.targetFunc = targetFunction;
        this.partialArgs = args;
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        TypeRef[] paramTypeRefs = new TypeRef[targetFunc.arity];
        for (int i = 0; i < paramTypeRefs.length; i++) paramTypeRefs[i] = new TypeRef();

        if (targetFunc.paramTypes != null) {
            for (int i=0; i < partialArgs.size(); i++) {
                try {
                    partialArgs.get(i).unify(targetFunc.paramTypes[i], env);
                } catch (UnifyException exc) {
                    throw UnifyException.addCause("Can't unify partial function argument with function arg",
                            exc);
                }
            }
            TypeRef[] remainingArgs = new TypeRef[targetFunc.arity - partialArgs.size()];
            for (int i=0; i < remainingArgs.length; i++) {
                remainingArgs[i] = targetFunc.paramTypes[i+partialArgs.size()];
            }
            FunctionType newFuncType = new FunctionType(remainingArgs.length, remainingArgs, targetFunc.returnType);
            try {
                typeRef.unify(new TypeRef(newFuncType));
            } catch (UnifyException exc) {
                throw UnifyException.addCause("Can't unify partial function with " + typeRef.getType(), exc);
            }
            return;
        }

        Environment<TypeRef> funcEnv = new Environment<>(env);
        for (int i = 0; i < paramTypeRefs.length; i++) {
            if (i < partialArgs.size()) {
                partialArgs.get(i).unify(paramTypeRefs[i], env);
            }
            String paramName = targetFunc.parameterList.get(i).value;
            funcEnv.define(paramName, paramTypeRefs[i]);
        }

        TypeRef returnType = new TypeRef();
        TypeRef lastExpr = new TypeRef();
        for (Expression expr: targetFunc.targetExpressions) {
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

        TypeRef[] partialTypeRefs = new TypeRef[targetFunc.arity - partialArgs.size()];
        for (int i=0; i < partialTypeRefs.length; i++) {
            partialTypeRefs[i] = paramTypeRefs[i+partialArgs.size()];
        }
        FunctionType thisType = new FunctionType(partialTypeRefs.length, partialTypeRefs, returnType);

        try {
            typeRef.unify(new TypeRef(thisType));
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Can't unify partial function with "+typeRef.getType(), exc);
        }
    }

    @Override
    public Expression apply(List<Expression> arguments, Environment<Expression> env)
        throws LispException {
        List<Expression> newArgs = new ArrayList<>();
        newArgs.addAll(partialArgs);
        newArgs.addAll(arguments);
        if (arguments.size() + partialArgs.size() < targetFunc.arity) {
            return new PartialApplicationExpr(targetFunc, newArgs);
        } else {
            return targetFunc.apply(newArgs, env);
        }
    }

    @Override
    public String toString() {
        return "(partial function)";
    }
}

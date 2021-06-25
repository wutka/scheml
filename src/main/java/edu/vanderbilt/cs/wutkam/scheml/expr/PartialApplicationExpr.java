package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.FunctionType;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheml.type.UnifyException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/** Represents the partial application of a function
 */
public class PartialApplicationExpr implements Expression, Applicable {
    FunctionExpr targetFunc;
    List<Expression> partialArgs;
    TypeRef[] paramTypes;
    TypeRef returnType;

    public PartialApplicationExpr(FunctionExpr targetFunction, List<Expression> args) {
        this.targetFunc = targetFunction;

        // Make a copy of the target function's types if they are present
        // because we could be holding on to the copy that came from the top level registry
        if (targetFunc.paramTypes != null) {
            Map<String,TypeRef> linkageMap = new HashMap<>();
            this.paramTypes = new TypeRef[targetFunc.paramTypes.length];
            for (int i=0; i < targetFunc.paramTypes.length; i++) {
                 this.paramTypes[i] = targetFunc.paramTypes[i].copy(linkageMap);
            }
            if (targetFunc.returnType != null) {
                this.returnType = targetFunc.returnType.copy(linkageMap);
            }
        }
        this.partialArgs = args;
    }

    // Since this class implements applicable, it will receive its parameters already evaluated, and
    // its partial argument had also already been evaluated, so there is no need to override the
    // default evaluate method here


    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {

        // If the target function's param types have already been determined, create a FunctionType representing
        // the remaining args, and unify against the requested type
        if (paramTypes != null) {

            for (int i=0; i < partialArgs.size(); i++) {
                partialArgs.get(i).unify(paramTypes[i], env);
            }
            // Get the param types for the remaining args
            TypeRef[] remainingArgs = new TypeRef[targetFunc.arity - partialArgs.size()];
            for (int i=0; i < remainingArgs.length; i++) {
                remainingArgs[i] = paramTypes[i+partialArgs.size()];
            }

            // The type of this function consists of the remaining args and the return type of the function
            FunctionType newFuncType = new FunctionType(remainingArgs.length, remainingArgs, this.returnType);
            try {
                // Unify this function type with the requested type
                typeRef.unify(new TypeRef(newFuncType));
            } catch (UnifyException exc) {
                throw UnifyException.addCause("Can't unify partial function with " + typeRef.getType(), exc);
            }
            return;
        }

        // If the function's param types haven't already been determined, create a new set of empty type refs
        TypeRef[] paramTypeRefs = new TypeRef[targetFunc.arity];
        for (int i = 0; i < paramTypeRefs.length; i++) paramTypeRefs[i] = new TypeRef();

        // Unify the partial args with the param type refs
        Environment<TypeRef> funcEnv = new Environment<>(env);
        for (int i = 0; i < paramTypeRefs.length; i++) {
            if (i < partialArgs.size()) {
                partialArgs.get(i).unify(paramTypeRefs[i], env);
            }
            // Store the param types in the function unification environment
            String paramName = targetFunc.parameterList.get(i).value;
            funcEnv.define(paramName, paramTypeRefs[i]);
        }

        TypeRef returnType = new TypeRef();

        // Unify the function expressions against the unification environment
        for (Expression expr: targetFunc.targetExpressions) {
            // Since the return type is that of the last expression, keep re-initializing it
            // so that at the end of the loop, it will have the type of the last expression
            returnType = new TypeRef();
            try {
                expr.unify(returnType, funcEnv);
            } catch (UnifyException exc) {
                throw UnifyException.addCause("Can't unify function body", exc);
            }
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
        // If there are still not enough arguments, return another partial function
        if (arguments.size() + partialArgs.size() < targetFunc.arity) {
            return new PartialApplicationExpr(targetFunc, newArgs);
        } else {
            // Otherwise, go ahead and apply the target function with the args
            return targetFunc.apply(newArgs, env);
        }
    }

    @Override
    public String toString() {
        return "(partial function)";
    }
}

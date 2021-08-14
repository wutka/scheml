package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.*;

import java.util.*;

/** Represents a function, and although it contains the implementation of applying the
 * function, an instance of this object just represents the function itself.
 */
public class FunctionExpr implements Expression, Applicable {
    /** The name of the function, or null if it is anonymous */
    public final String name;

    /** The arity of the function (number of arguments) */
    public final int arity;

    /** The expressions that get executed when this function executes */
    public final List<Expression> targetExpressions;

    /** The names of each function parameter */
    public final List<SymbolExpr> parameterList;

    /** If this function was created in the context of a closure it has an environment associated with it */
    protected Environment<Expression> closureEnv;

    // For some functions, the parameter types and return type are already known
    public TypeRef[] paramTypes;
    public TypeRef returnType;

    public FunctionExpr(String name, int arity, List<SymbolExpr> parameterList, List<Expression> targetExpressions) {
        this.name = name;
        this.arity = arity;
        this.parameterList = parameterList;
        this.targetExpressions = targetExpressions;
    }

    public FunctionExpr(String name, String signature) {
        this.name = name;
        Type type;
        try {
            type = Type.parseTypeSignature(signature);
        } catch (LispException exc) {
            throw new RuntimeException("Error parsing signature: "+signature, exc);
        }
        if (!(type instanceof FunctionType)) {
            throw new RuntimeException("Signature "+signature+" is not a function");
        }
        FunctionType functionType = (FunctionType) type;
        this.arity = functionType.arity;
        this.parameterList = null;
        this.targetExpressions = null;
        this.paramTypes = functionType.paramTypes;
        this.returnType = functionType.returnType;
    }

    public FunctionExpr(String name, List<TypeRef> paramTypesList, TypeRef returnType) {
        this.name = name;
        this.arity = paramTypesList.size();
        this.parameterList = null;
        this.targetExpressions = null;
        this.paramTypes = paramTypesList.toArray(new TypeRef[paramTypesList.size()]);
        this.returnType = returnType;
    }

    public FunctionExpr(FunctionExpr other) {
        this.name = other.name;
        this.arity = other.arity;
        this.parameterList = other.parameterList;
        this.targetExpressions = other.targetExpressions;
        this.paramTypes = other.paramTypes;
        this.returnType = other.returnType;
        this.closureEnv = other.closureEnv;
    }

    @Override
    public String toString() {
        if (name == null) {
            return "(function)";
        } else {
            return "(function "+name+")";
        }
    }

    /** Applies a function. It is assumed that the function arguments have already been evaluated */
    public Expression apply(List<Expression> arguments, Environment<Expression> env)
        throws LispException {
        if (arguments.size() > arity) {
            throw new LispException("Too many parameters passed to function "+this);
        }

        // If there aren't enough arguments, turn this application into a partial application
        if (arguments.size() < arity) {
            return new PartialApplicationExpr(this, arguments);
        }

        // Create a new environment and assign the value of each argument to the name of the argument.
        // For example, here's a simple area function:
        // (define (area width height) (* width height))
        // When applying the area function, this function receives 2 arguments
        // It creates a new environment and stores the first argument in the environment
        // under the name "width", and the second argument under the name "height".
        // The (* width height) part of the function definition is the target expression. When that
        // expression is evaluated, it is passed the updated environment that contains the "width" and
        // "height" values. When the width symbol in the target expression is evaluated, it looks it
        // up "width" in the environment, and the same with the height symbol, and then both those
        // values are passed to the * function to compute the result.
        Environment<Expression> funcEnv = new Environment<>(closureEnv);
        if (parameterList != null) {
            for (int i=0; i < arity; i++) {
                funcEnv.define(parameterList.get(i).value, arguments.get(i));
            }
        }

        // We allow a function to contain multiple expressions, which is a little more like an imperative
        // language. The last expression is in the tail position
        Expression last = null;
        for (int i=0; i < targetExpressions.size(); i++) {
            Expression target = targetExpressions.get(i);
            last = target.evaluate(funcEnv, i == targetExpressions.size()-1);
        }

        // The return value of the function is just the value computed by the last target expression
        return last;
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        // paramTypeRefs represent the types of each parameter as determined by their usage in the body
        TypeRef[] paramTypeRefs = new TypeRef[arity];
        for (int i = 0; i < paramTypeRefs.length; i++) paramTypeRefs[i] = new TypeRef();

        // But, if paramTypes have already been provided, unify those with paramTypeRefs
        if (paramTypes != null) {
            for (int i=0; i < paramTypeRefs.length; i++) {
                paramTypeRefs[i].unify(paramTypes[i]);
            }
        }

        TypeRef returnType = this.returnType;

        // If returnType hasn't already been provided, create an empty one
        if (returnType == null) returnType = new TypeRef();

        // Create an environment for unification, which is a map from symbols to type refs.
        // Store each param type ref in the environment. Similar to the environment for evaluation,
        // when unifying the function body it will use this environment to resolves the symbols
        // representing function parameters
        Environment<TypeRef> funcEnv = new Environment<>(env);
        if (parameterList != null) {
            for (int i = 0; i < arity; i++) {
                String paramName = parameterList.get(i).value;
                funcEnv.define(paramName, paramTypeRefs[i]);
            }
        }

        if (targetExpressions != null) {
            TypeRef lastExpr = new TypeRef();
            for (Expression expr : targetExpressions) {
                // Since there is no dependence between each statement in the target expression list,
                // reset the last expression typeref to an empty ref
                lastExpr = new TypeRef();
                try {
                    // Unify this expression with the function environment
                    expr.unify(lastExpr, funcEnv);
                } catch (UnifyException exc) {
                    throw UnifyException.addCause("Can't unify function body", exc);
                }
            }
            // Unify the return type with the type of the last expression, since it is used for the return value
            try {
                returnType.unify(lastExpr);
            } catch (UnifyException exc) {
                throw UnifyException.addCause("Can't unify function return type", exc);
            }
        }

        // Create a new FunctionType that represents this function
        FunctionType thisType = new FunctionType(paramTypeRefs.length, paramTypeRefs, returnType);

        // Unify it with the type that was passed in
        try {
            typeRef.unify(new TypeRef(thisType));
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Can't unify function with "+typeRef.getType(), exc);
        }
    }

    @Override
    public Expression toScheml() {
        return new SymbolLiteralExpr(name);
    }
}

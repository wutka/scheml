package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.*;

import java.io.PushbackReader;
import java.io.StringReader;
import java.util.*;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/25/21
 * Time: 2:07 PM
 */
public class FunctionExpr implements Expression, Applicable {
    public final int arity;
    public final List<Expression> targetExpressions;
    public final List<SymbolExpr> parameterList;

    public TypeRef[] paramTypes;
    public TypeRef returnType;

    public FunctionExpr(int arity, List<SymbolExpr> parameterList, List<Expression> targetExpressions) {
        this.arity = arity;
        this.parameterList = parameterList;
        this.targetExpressions = targetExpressions;
    }

    public FunctionExpr(String signature) {
        Type type = null;
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

    public FunctionExpr(List<TypeRef> paramTypesList, TypeRef returnType) {
        this.arity = paramTypesList.size();
        this.parameterList = null;
        this.targetExpressions = null;
        this.paramTypes = paramTypesList.toArray(new TypeRef[paramTypesList.size()]);
        this.returnType = returnType;
    }

    @Override
    public String toString() {
        return "(function)";
    }

    public Expression apply(List<Expression> arguments, Environment<Expression> env)
        throws LispException {
        if (arguments.size() > arity) {
            throw new LispException("Too many parameters passed to function "+this);
        }

        if (arguments.size() < arity) {
            return new PartialApplicationExpr(this, arguments);
        }

        Environment<Expression> funcEnv = new Environment<>(env);
        if (parameterList != null) {
            for (int i=0; i < arity; i++) {
                funcEnv.define(parameterList.get(i).value, arguments.get(i));
            }
        }
        Expression last = null;
        for (int i=0; i < targetExpressions.size(); i++) {
            Expression target = targetExpressions.get(i);
            last = target.evaluate(funcEnv, i == targetExpressions.size()-1);
        }

        return last;
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        TypeRef[] paramTypeRefs = new TypeRef[arity];
        for (int i = 0; i < paramTypeRefs.length; i++) paramTypeRefs[i] = new TypeRef();

        if (paramTypes != null) {
            for (int i=0; i < paramTypeRefs.length; i++) {
                paramTypeRefs[i].unify(paramTypes[i]);
            }
        }
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
        }

        FunctionType thisType = new FunctionType(paramTypeRefs.length, paramTypeRefs, returnType);

        try {
            typeRef.unify(new TypeRef(thisType));
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Can't unify function with "+typeRef.getType(), exc);
        }
    }
}

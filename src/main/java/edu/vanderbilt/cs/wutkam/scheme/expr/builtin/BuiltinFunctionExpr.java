package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.*;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.FunctionType;
import edu.vanderbilt.cs.wutkam.scheme.type.Type;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheme.type.UnifyException;

import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/26/21
 * Time: 11:21 AM
 */
public class BuiltinFunctionExpr extends FunctionExpr {
    public String name;
    public BuiltinFunction builtinFunc;

    protected BuiltinFunctionExpr(String name, String signature) {
        super(signature);
        this.name = name;
    }

    public BuiltinFunctionExpr(String name, String signature, BuiltinFunction builtinFunc) {
        super(signature);
        this.name = name;
        this.builtinFunc = builtinFunc;
    }

    public BuiltinFunctionExpr(BuiltinFunctionExpr origFunc, List<Expression> partialArgs) {
        super(origFunc, partialArgs);
        this.builtinFunc = origFunc.builtinFunc;
        this.paramTypes = origFunc.paramTypes;
        this.returnType = origFunc.returnType;
    }

    protected Expression apply(List<Expression> arguments, Environment<Expression> env)
            throws LispException {
        if (arguments.size() + partialArgs.size() > arity) {
            throw new LispException("Too many parameters passed to function "+this);
        }

        if (arguments.size() + partialArgs.size() == arity) {
            Expression[] args = new Expression[arity];
            for (int i=0; i < arity; i++) {
                if (i < partialArgs.size()) {
                    args[i] = partialArgs.get(i).evaluate(env);
                } else {
                    args[i] = arguments.get(i-partialArgs.size()).evaluate(env);
                }
            }
            if (this.partialFunc != null) {
                if (this.partialFunc instanceof BuiltinFunctionExpr) {
                    return ((BuiltinFunctionExpr) this.partialFunc).executeBuiltin(args);
                } else {
                    throw new LispException("Unable to execute partial builtin function");
                }
            } else {
                return executeBuiltin(args);
            }
        } else {
            return new BuiltinFunctionExpr(this, arguments);
        }
    }

    protected Expression executeBuiltin(Expression[] args) throws LispException {
        return builtinFunc.apply(args);
    }

    protected Expression fromJavaValue(Object obj) {
        if (obj instanceof Boolean) {
            return new BoolExpr((Boolean) obj);
        } else if (obj instanceof Character) {
            return new CharExpr((Character) obj);
        } else if (obj instanceof Double) {
            return new DoubleExpr((Double) obj);
        } else if (obj instanceof Integer) {
            return new IntExpr((Integer) obj);
        } else if (obj instanceof String) {
            return new StringExpr((String) obj);
        }
        return (Expression) obj;
    }


    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        for (int i = 0; i < partialArgs.size(); i++) {
            try {
                partialArgs.get(i).unify(paramTypes[i], env);
            } catch (UnifyException exc) {
                throw UnifyException.addCause("Cannot unify function parameter " + paramTypes[i].getType(), exc);
            }
        }

        TypeRef[] newParamTypeRefs = new TypeRef[arity - partialArgs.size()];
        for (int i=0; i < newParamTypeRefs.length; i++) {
            newParamTypeRefs[i] = paramTypes[i+partialArgs.size()];
        }
        FunctionType thisType = new FunctionType(newParamTypeRefs.length, newParamTypeRefs, returnType);
        try {
            typeRef.unify(new TypeRef(thisType));
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Can't unify function with "+typeRef.getType(), exc);
        }
    }

}

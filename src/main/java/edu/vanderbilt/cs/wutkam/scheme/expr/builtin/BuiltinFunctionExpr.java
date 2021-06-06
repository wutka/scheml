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
    public BuiltinFunction builtinFunc;

    protected BuiltinFunctionExpr(String name, String signature) {
        super(name, signature);
    }

    public BuiltinFunctionExpr(String name, String signature, BuiltinFunction builtinFunc) {
        super(name, signature);
        this.builtinFunc = builtinFunc;
    }

    public Expression apply(List<Expression> arguments, Environment<Expression> env)
            throws LispException {
        if (arguments.size() > arity) {
            throw new LispException("Too many parameters passed to function "+this);
        }

        if (arguments.size() < arity) {
            return new PartialApplicationExpr(this, arguments);
        }

        Expression[] args = new Expression[arity];
        for (int i=0; i < arity; i++) {
            args[i] = arguments.get(i).evaluate(env, false);
        }
        return executeBuiltin(args);
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

        FunctionType thisType = new FunctionType(arity, paramTypes, returnType);
        try {
            typeRef.unify(new TypeRef(thisType));
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Can't unify function with "+typeRef.getType(), exc);
        }
    }

}

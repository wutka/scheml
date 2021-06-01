package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;

public class BuiltinUnaryFunctionExpr<A,B>
        extends BuiltinFunctionExpr {
    protected UnaryFunction<A,B> implFunc;

    public BuiltinUnaryFunctionExpr(String name, String signature, UnaryFunction<A,B> implFunc) {
        super(name, signature);
        this.implFunc = implFunc;

        if (arity != 1) {
            throw new RuntimeException("Arity on function "+name+" should be 1, check signature");
        }
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        return fromJavaValue((B) implFunc.apply((A) args[0]));
    }
}

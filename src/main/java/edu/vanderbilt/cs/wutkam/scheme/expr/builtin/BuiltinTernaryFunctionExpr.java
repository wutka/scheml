package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;

public class BuiltinTernaryFunctionExpr<A,B,C, D> extends BuiltinFunctionExpr {
    protected TernaryFunction<A,B,C,D> implFunc;

    public BuiltinTernaryFunctionExpr(String name, String signature, TernaryFunction<A,B,C,D> implFunc) {
        super(name, signature);
        this.implFunc = implFunc;

        if (arity != 3) {
            throw new RuntimeException("Arity on function "+name+" should be 3, check signature");
        }
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        return fromJavaValue((D) implFunc.apply((A) args[0], (B) args[1], (C) args[2]));
    }
}

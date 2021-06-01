package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;

public class BuiltinBinaryFunctionExpr<A,B,C>
        extends BuiltinFunctionExpr {
    protected BinaryFunction<A,B,C> implFunc;

    public BuiltinBinaryFunctionExpr(String name, String signature, BinaryFunction<A,B,C> implFunc) {
        super(name, signature);
        this.implFunc = implFunc;
        if (arity != 2) {
            throw new RuntimeException("Arity on function "+name+" should be 2, check signature");
        }
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        return fromJavaValue((C) implFunc.apply((A) args[0].toJavaValue(), (B) args[1].toJavaValue()));
    }
}

package edu.vanderbilt.cs.wutkam.scheml.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;

/** Represents a builtin function that takes 3 arguments of types A, B, and C
 * and returns a result of type D
 */
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
        return fromJavaValue(implFunc.apply((A) args[0].toJavaValue(), (B) args[1].toJavaValue(), (C) args[2].toJavaValue()));
    }
}

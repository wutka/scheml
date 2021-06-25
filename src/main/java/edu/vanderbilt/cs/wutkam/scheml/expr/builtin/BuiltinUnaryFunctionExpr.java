package edu.vanderbilt.cs.wutkam.scheml.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;

/** Represents a builtin unary function that takes an argument of type A and returns and argument of type B
 */
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
        return fromJavaValue(implFunc.apply((A) args[0].toJavaValue()));
    }
}

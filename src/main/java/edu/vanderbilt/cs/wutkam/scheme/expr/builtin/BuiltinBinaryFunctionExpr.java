package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;

/** A wrapper to make it easy to create binary functions with lambdas.
 */
public class BuiltinBinaryFunctionExpr<A,B,C>
        extends BuiltinFunctionExpr {
    protected BinaryFunction<A,B,C> implFunc;

    /** Create a new binary function with the specified implementation. For example:
     * new BuiltinBinaryFunctionExpr<>("+", "int -> int -> int", (Integer a, Integer b) -> a+b)
     *
     * @param name The name of the function as it is called from the interpreter
     * @param signature A function signature in the form (arg type -> arg type -> return type)
     * @param implFunc The implementation for this function (usually a lambda)
     */
    public BuiltinBinaryFunctionExpr(String name, String signature, BinaryFunction<A,B,C> implFunc) {
        super(name, signature);
        this.implFunc = implFunc;
        if (arity != 2) {
            throw new RuntimeException("Arity on function "+name+" should be 2, check signature");
        }
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // Use toJavaValue() to convert to a native java value, e.g. BooleanExpr -> Boolean
        return fromJavaValue(implFunc.apply((A) args[0].toJavaValue(), (B) args[1].toJavaValue()));
    }
}

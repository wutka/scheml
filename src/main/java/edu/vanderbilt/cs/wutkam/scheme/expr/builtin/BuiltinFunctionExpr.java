package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.*;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.FunctionType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheme.type.UnifyException;

import java.util.List;

/** A base type for built-in functions that evaluates all its argument expressions and then
 * invokes a built-in function expression.
 */
public abstract class BuiltinFunctionExpr extends FunctionExpr {

    /** Create a BuiltinFunctionExpr with the specified name and signature
     *
     * @param name The name of the function as called from the interpreter
     * @param signature The function signature in the form: arg type -> arg type -> ... -> arg type -> return type
     */
    protected BuiltinFunctionExpr(String name, String signature) {
        super(name, signature);
    }

    /** Apply the function
     *
     * @param arguments The function arguments
     * @param env The function evaluation environment
     * @return
     * @throws LispException
     */
    public Expression apply(List<Expression> arguments, Environment<Expression> env)
            throws LispException {
        if (arguments.size() > arity) {
            throw new LispException("Too many parameters passed to function "+this);
        }

        // If there aren't enough arguments supplied, return a partial function that will
        // invoke this function when all the arguments have been supplied
        if (arguments.size() < arity) {
            return new PartialApplicationExpr(this, arguments);
        }

        // Evaluate all the arguments
        Expression[] args = new Expression[arity];
        for (int i=0; i < arity; i++) {
            args[i] = arguments.get(i);
        }

        // Invoke the built-in function
        return executeBuiltin(args);
    }

    protected abstract Expression executeBuiltin(Expression[] args) throws LispException;

    /** Converts a Java type into an Expression, this is only for basic data types */
    protected Expression fromJavaValue(Object obj) {
        if (obj instanceof Boolean) {
            return new BoolExpr((Boolean) obj);
        } else if (obj instanceof Character) {
            return new CharExpr((Character) obj);
        } else if (obj instanceof Double) {
            return new DoubleExpr((Double) obj);
        } else if (obj instanceof Long) {
            return new IntExpr((Long) obj);
        } else if (obj instanceof String) {
            return new StringExpr((String) obj);
        }
        return (Expression) obj;
    }


    /** Perform type unification for the function type. Since the argument types are already
     * known, we don't unify on the expression values here. That will be done when the function
     * is applied.
     * @param typeRef The type reference to unify with
     * @param env The current evaluation environment
     * @throws LispException
     */
    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {

        // Create a function type representing this function's param types and return types
        FunctionType thisType = new FunctionType(arity, paramTypes, returnType);
        try {
            // Unify with the requested type
            typeRef.unify(new TypeRef(thisType));
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Can't unify function with "+typeRef.getType(), exc);
        }
    }

}

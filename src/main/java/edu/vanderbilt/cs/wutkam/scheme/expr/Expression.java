package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

/** Represents any expression, which must support being evaluated and unified with a type reference,
 * and also conversion to a Java value for basic types.
 */
public interface Expression {
    default Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        return this;
    }

    void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException;

    default Object toJavaValue() { return this; }
}

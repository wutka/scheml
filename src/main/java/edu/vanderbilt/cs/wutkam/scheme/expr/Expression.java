package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/24/21
 * Time: 12:58 PM
 */

public interface Expression {
    default Expression evaluate(Environment<Expression> env) throws LispException {
        return this;
    }

    void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException;
}

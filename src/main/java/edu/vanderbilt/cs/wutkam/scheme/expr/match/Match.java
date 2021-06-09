package edu.vanderbilt.cs.wutkam.scheme.expr.match;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

/**
 */
public interface Match {
    boolean matches(Expression expression);
    void defineEnvironment(Expression expr, Environment<Expression> env);

    void unify(TypeRef matchTargetType, Environment<TypeRef> env) throws LispException;
}

package edu.vanderbilt.cs.wutkam.scheme.expr.match;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

/** Represents the part of a match expression that is what to match
 */
public interface Match {
    /** Returns true if the given expression matches this matcher*/
    boolean matches(Expression expression);

    /** If this matcher contains variables, set variables in the given environment
     * based on what parts of the given expression are matched
     */
    void defineEnvironment(Expression expr, Environment<Expression> env);

    /** Unifies this matcher with the specified type */
    void unify(TypeRef matchTargetType, Environment<TypeRef> env) throws LispException;
}

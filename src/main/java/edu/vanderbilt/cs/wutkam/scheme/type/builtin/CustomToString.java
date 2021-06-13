package edu.vanderbilt.cs.wutkam.scheme.type.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.AbstractTypeExpr;

/** Indicates that this AbstractTypeDecl implements a custom toString for its expressions
 */
public interface CustomToString {
    String customToString(AbstractTypeExpr expr);
}

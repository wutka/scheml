package edu.vanderbilt.cs.wutkam.scheml.type.builtin;

import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;

/** Indicates that this AbstractTypeDecl implements a custom toString for its expressions
 */
public interface CustomToString {
    String customToString(AbstractTypeExpr expr);
}

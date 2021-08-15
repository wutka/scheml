package edu.vanderbilt.cs.wutkam.scheml.type.builtin;

import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;

public interface CustomToScheml {
    public Expression customToScheml(AbstractTypeExpr expr);
}

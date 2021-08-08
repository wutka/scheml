package edu.vanderbilt.cs.wutkam.scheml.type.builtin;

import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;

public interface CustomToJavaValue {
    Object toJavaValue(AbstractTypeExpr expr);
}

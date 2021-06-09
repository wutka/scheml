package edu.vanderbilt.cs.wutkam.scheme.type.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.AbstractTypeExpr;

/**
 */
public interface CustomToString {
    String customToString(AbstractTypeExpr expr);
}

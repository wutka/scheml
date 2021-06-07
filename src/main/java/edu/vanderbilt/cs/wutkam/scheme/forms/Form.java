package edu.vanderbilt.cs.wutkam.scheme.forms;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.ListExpr;

/** Represents a special form, a syntactic structure in the language that cannot be
 * implemented as a function because of its evaluation requirements. For instance,
 * (if test true-option false-option) should be implemented as a special form because
 * false-option should not be evaluated if the test returns true.
 */
public interface Form {
    Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException;
}

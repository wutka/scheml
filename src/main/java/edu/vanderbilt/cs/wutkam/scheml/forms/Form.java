package edu.vanderbilt.cs.wutkam.scheml.forms;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.ListExpr;

/** Represents a special form, a syntactic structure in the language that cannot be
 * implemented as a function because of its evaluation requirements. For instance,
 * (if test true-option false-option) should be implemented as a special form because
 * false-option should not be evaluated if the test returns true.
 */
public interface Form {
    Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException;
}

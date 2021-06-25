package edu.vanderbilt.cs.wutkam.scheml.forms;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.IfExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.ListExpr;

/** Expands the form (if test true-option false-option) into an IfExpr expression
 */
public class IfForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        if (aList.size() != 4) {
            throw new LispException("if expression must have both true and false paths");
        }

        Expression testExpr = aList.getElement(1);
        if (testExpr instanceof ListExpr) {
            testExpr = FormExpander.expand((ListExpr) testExpr, false);
        }

        Expression trueExpr = aList.getElement(2);
        if (trueExpr instanceof ListExpr) {
            trueExpr = FormExpander.expand((ListExpr) trueExpr, false);
        }

        Expression falseExpr = aList.getElement(3);
        if (falseExpr instanceof ListExpr) {
            falseExpr = FormExpander.expand((ListExpr) falseExpr, false);
        }

        return new IfExpr(testExpr, trueExpr, falseExpr);
    }
}

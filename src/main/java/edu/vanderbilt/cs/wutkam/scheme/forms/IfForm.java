package edu.vanderbilt.cs.wutkam.scheme.forms;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.IfExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.ListExpr;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/24/21
 * Time: 4:34 PM
 */
public class IfForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        if (aList.elements.size() != 4) {
            throw new LispException("if expression must have both true and false paths");
        }

        Expression testExpr = aList.elements.get(1);
        if (testExpr instanceof ListExpr) {
            testExpr = FormExpander.expand((ListExpr) testExpr, false);
        }

        Expression trueExpr = aList.elements.get(2);
        if (trueExpr instanceof ListExpr) {
            trueExpr = FormExpander.expand((ListExpr) trueExpr, false);
        }

        Expression falseExpr = aList.elements.get(3);
        if (falseExpr instanceof ListExpr) {
            falseExpr = FormExpander.expand((ListExpr) falseExpr, false);
        }

        return new IfExpr(testExpr, trueExpr, falseExpr);
    }
}

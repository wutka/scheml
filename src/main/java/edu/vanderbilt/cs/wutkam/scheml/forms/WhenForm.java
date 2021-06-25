package edu.vanderbilt.cs.wutkam.scheml.forms;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.WhenExpr;

import java.util.ArrayList;
import java.util.List;

/** Expands the form (when test true-option) into a WhenExpr expression
 */
public class WhenForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        if (aList.size() < 3) {
            throw new LispException("when expression must a test and a series of statements");
        }

        Expression testExpr = aList.getElement(1);
        if (testExpr instanceof ListExpr) {
            testExpr = FormExpander.expand((ListExpr) testExpr, false);
        }

        List<Expression> body = new ArrayList<>();

        // Expand each body expression if necessary
        for (Expression expr: aList.elementsFrom(2)) {
            if (expr instanceof ListExpr) {
                expr = FormExpander.expand((ListExpr) expr, false);
            }
            body.add(expr);
        }

        return new WhenExpr(testExpr, body);
    }
}

package edu.vanderbilt.cs.wutkam.scheml.forms;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.StringExpr;

import java.util.ArrayList;
import java.util.List;

/** If a three-argument assert is missing the third argument (the message), add a blank message */
public class AssertThreeArgsForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        List<Expression> exprs = new ArrayList<>();
        for (Expression expr: aList.elementsFrom(0)) {
            if (expr instanceof ListExpr) {
                expr = FormExpander.expand((ListExpr) expr, false);
            }
            exprs.add(expr);
        }
        if (exprs.size() == 3) {
            exprs.add(new StringExpr(""));
        }
        return new ListExpr(exprs);
    }
}

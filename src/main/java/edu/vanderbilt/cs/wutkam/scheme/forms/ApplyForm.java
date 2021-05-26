package edu.vanderbilt.cs.wutkam.scheme.forms;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.ApplyExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.SymbolExpr;

import java.util.ArrayList;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/25/21
 * Time: 4:41 PM
 */
public class ApplyForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        Expression targetExpression = aList.elements.get(0);
        if (targetExpression instanceof ListExpr) {
            targetExpression = FormExpander.expand((ListExpr) targetExpression, false);
        }

        ArrayList<Expression> expanded = new ArrayList<>(aList.elements.subList(1, aList.elements.size()));
        for (int i=0; i < expanded.size(); i++) {
            Expression expr = expanded.get(i);
            if (expr instanceof ListExpr) {
                expanded.set(i, FormExpander.expand((ListExpr) expr, false));
            }
        }
        return new ApplyExpr(targetExpression, expanded);
    }
}

package edu.vanderbilt.cs.wutkam.scheml.forms;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.PrintfExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.SymbolExpr;

import java.util.ArrayList;
import java.util.List;

/** Provides printf/sprintf special forms
 */
public class PrintfForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        String formName = ((SymbolExpr)aList.getElement(0)).value;
        boolean returnString = formName.equals("sprintf");

        if (aList.size() < 2) {
            throw new LispException("printf/sprintf form requires at least a format string");
        }

        // Expand the format expression if necessary
        Expression formatExpr = aList.getElement(1);
        if (!(formatExpr instanceof StringExpr)) {
            throw new LispException("Format string for " + formName + " must be a string constant, not an expression");
        }
        String formatString = ((StringExpr) formatExpr).value;

        // Expand each param
        List<Expression> params = new ArrayList<>();
        for (Expression expr: aList.elementsFrom(2)) {
            if (expr instanceof ListExpr) {
                params.add(FormExpander.expand((ListExpr) expr, false));
            } else {
                params.add(expr);
            }
        }
        return new PrintfExpr(returnString, formatString, params);
    }
}

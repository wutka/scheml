package edu.vanderbilt.cs.wutkam.scheml.forms;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.MakeSetExpr;

import java.util.ArrayList;
import java.util.List;

/** Allows for a variable-argument (make-set v1 v2 v3 ...) form that is turned into a MakeSetExpr */
public class MakeSetForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        List<Expression> expanded = new ArrayList<>();

        // Expand each element in the list if necessary
        for (Expression expr: aList.elementsFrom(1)) {
            if (expr instanceof ListExpr) {
                expr = FormExpander.expand((ListExpr) expr, false);
            }
            expanded.add(expr);
        }
        return new MakeSetExpr(expanded);
    }
}
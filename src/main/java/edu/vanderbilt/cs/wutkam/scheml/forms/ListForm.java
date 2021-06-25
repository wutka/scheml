package edu.vanderbilt.cs.wutkam.scheml.forms;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.GeneratedConsExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.ListExpr;

import java.util.ArrayList;
import java.util.List;

/** Allows for a variable-argument (list x y z ...) form that is turned into a GeneratedConsExpr */
public class ListForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        List<Expression> expanded = new ArrayList<>();

        // Expand each element in the list if necessary
        for (Expression expr: aList.elementsFrom(1)) {
            if (expr instanceof ListExpr) {
                expanded.add(FormExpander.expand((ListExpr) expr, false));
            } else {
                expanded.add(expr);
            }
        }
        return new GeneratedConsExpr(expanded);
    }
}

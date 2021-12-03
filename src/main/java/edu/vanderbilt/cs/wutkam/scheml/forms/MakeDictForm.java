package edu.vanderbilt.cs.wutkam.scheml.forms;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.MakeDictExpr;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/** Allows for a variable-argument (make-dict (k1 v1) (k2 v2) (k3 v3) ...) form that is turned into a MakeDictExpr */
public class MakeDictForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        List<Expression> expanded = new ArrayList<>();

        // Expand each element in the list if necessary
        for (Expression expr: aList.elementsFrom(1)) {
            if (expr instanceof ListExpr) {
                ListExpr kvExpr = (ListExpr) expr;
                if (kvExpr.size() != 2) {
                    throw new LispException("make-dict parameters should be key-value pairs as 2-element lists");
                }
                Expression keyExpr = kvExpr.getElement(0);
                if (keyExpr instanceof ListExpr) {
                    keyExpr = FormExpander.expand((ListExpr) keyExpr, false);

                }
                Expression valueExpr = kvExpr.getElement(1);
                if (valueExpr instanceof ListExpr) {
                    valueExpr = FormExpander.expand((ListExpr) valueExpr, false);
                }
                expanded.add(new ListExpr(Arrays.asList(keyExpr, valueExpr)));
            } else {
                throw new LispException("make-dict parameters should be key-value pairs as 2-element lists");
            }
        }
        return new MakeDictExpr(expanded);
    }
}
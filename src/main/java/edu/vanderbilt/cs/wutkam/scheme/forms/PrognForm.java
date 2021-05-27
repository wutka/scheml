package edu.vanderbilt.cs.wutkam.scheme.forms;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.PrognExpr;

import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/26/21
 * Time: 6:31 PM
 */
public class PrognForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        if (aList.elements.size() < 2) {
            throw new LispException("Progn needs at least one statement");
        }
        List<Expression> body = new ArrayList<>();
        for (Expression expr: aList.elements.subList(1, aList.elements.size())) {
            if (expr instanceof ListExpr) {
                expr = FormExpander.expand((ListExpr) expr, false);
            }
            body.add(expr);
        }
        return new PrognExpr(body);
    }
}

package edu.vanderbilt.cs.wutkam.scheme.forms;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.ConsExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.QuoteExpr;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/26/21
 * Time: 2:35 PM
 */
public class QuoteForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        if (aList.elements.size() < 2) {
            throw new LispException("Quote form requires a single argument");
        }
        Expression expr = aList.elements.get(1);
        if (expr instanceof ListExpr) {
            expr = new ConsExpr(((ListExpr)expr).elements);
        }
        return new QuoteExpr(expr);
    }
}

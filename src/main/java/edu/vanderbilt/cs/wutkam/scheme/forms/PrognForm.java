package edu.vanderbilt.cs.wutkam.scheme.forms;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.PrognExpr;

import java.util.ArrayList;
import java.util.List;

/** Provides a progn form allowing for multiple statements, where the return type of the block is
 * the return type of the last statement. While let and lambda already allow for blocks, this
 * might be needed for an if statement or a match statement.
 */
public class PrognForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        if (aList.size() < 2) {
            throw new LispException("Progn needs at least one statement");
        }

        List<Expression> body = new ArrayList<>();

        // Expand each body expression if necessary
        for (Expression expr: aList.elementsFrom(1)) {
            if (expr instanceof ListExpr) {
                expr = FormExpander.expand((ListExpr) expr, false);
            }
            body.add(expr);
        }
        return new PrognExpr(body);
    }
}

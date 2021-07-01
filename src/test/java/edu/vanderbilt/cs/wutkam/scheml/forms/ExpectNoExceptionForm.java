package edu.vanderbilt.cs.wutkam.scheml.forms;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.ExpectNoExceptionExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.StringExpr;

import java.util.ArrayList;
import java.util.List;

public class ExpectNoExceptionForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        String message = null;
        // If no message, body starts at item 2
        int offset = 1;

        if (aList.getElement(1) instanceof StringExpr) {
            message = ((StringExpr)aList.getElement(1)).value;

            // Since there is a message, the body starts at item 2
            offset = 2;
        }

        List<Expression> body = new ArrayList<>();
        for (Expression expr: aList.elementsFrom(offset)) {
            if (expr instanceof ListExpr) {
                expr = FormExpander.expand((ListExpr) expr, false);
            }
            body.add(expr);
        }
        return new ExpectNoExceptionExpr(message, body);
    }
}

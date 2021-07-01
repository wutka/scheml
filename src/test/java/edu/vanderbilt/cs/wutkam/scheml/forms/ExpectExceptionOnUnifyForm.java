package edu.vanderbilt.cs.wutkam.scheml.forms;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.ExpectExceptionOnUnifyExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.SymbolExpr;

import java.util.ArrayList;
import java.util.List;

public class ExpectExceptionOnUnifyForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        if (!(aList.getElement(1) instanceof SymbolExpr)) {
            throw new LispException("The first argument to expect-expression must be a symbol");
        }
        String exceptionName = ((SymbolExpr)aList.getElement(1)).value;

        String message = null;
        // If no message, body starts at item 2
        int offset = 2;

        if (aList.getElement(2) instanceof StringExpr) {
            message = ((StringExpr)aList.getElement(2)).value;

            // Since there is a message, the body starts at item 3
            offset = 3;
        }

        List<Expression> body = new ArrayList<>();
        for (Expression expr: aList.elementsFrom(offset)) {
            if (expr instanceof ListExpr) {
                expr = FormExpander.expand((ListExpr) expr, false);
            }
            body.add(expr);
        }
        return new ExpectExceptionOnUnifyExpr(exceptionName, message, body);
    }
}

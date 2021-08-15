package edu.vanderbilt.cs.wutkam.scheml.forms;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.EvalExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.ListExpr;

/** Expands the form (eval expr) into an EvalExpr expression
 */
public class EvalForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        if (aList.size() != 2) {
            throw new LispException("eval expression must have one expression");
        }

        Expression evalExpr = aList.getElement(1);
        if (evalExpr instanceof ListExpr) {
            evalExpr = FormExpander.expand((ListExpr) evalExpr, false);
        }
        return new EvalExpr(evalExpr);
    }
}

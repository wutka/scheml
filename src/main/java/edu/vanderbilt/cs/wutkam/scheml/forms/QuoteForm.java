package edu.vanderbilt.cs.wutkam.scheml.forms;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.QuoteExpr;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.SexprTypeDecl;

public class QuoteForm implements Form {
    @Override
    public Expression expandForm(ListExpr listExpr, boolean isTopLevel) throws LispException {
        return new QuoteExpr((ListExpr)listExpr.getElement(1));
    }
}

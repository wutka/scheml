package edu.vanderbilt.cs.wutkam.scheml.forms;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.SexprTypeDecl;

public class ExpandForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        Expression expanded = FormExpander.expand(new ListExpr(aList.elementsFrom(1)), false);
        if (expanded instanceof ListExpr) {
            ListExpr l = (ListExpr) expanded;
            if (l.size() == 1) {
                expanded = l.getElement(0);
            }
        }
        return SexprTypeDecl.fromExpression(expanded.toScheml(), new Environment<>());
    }
}

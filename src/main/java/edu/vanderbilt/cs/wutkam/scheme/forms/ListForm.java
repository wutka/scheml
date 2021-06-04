package edu.vanderbilt.cs.wutkam.scheme.forms;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.ConsExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.ListExpr;

public class ListForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        ConsExpr curr = new ConsExpr();
        for (int i=aList.size()-1; i >= 1; i--) {
            curr = new ConsExpr(aList.getElement(i), curr);
        }
        return curr;
    }
}

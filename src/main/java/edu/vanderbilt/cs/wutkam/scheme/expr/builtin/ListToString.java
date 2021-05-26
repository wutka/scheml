package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.CharExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.ConsExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.StringExpr;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/26/21
 * Time: 1:50 PM
 */
public class ListToString extends BuiltinFunctionExpr {
    public ListToString(String name) {
        super(name, 1);
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        if (!(args[0] instanceof ConsExpr)) {
            throw new LispException("First argument to "+name+" must be a list");
        }
        StringBuilder builder = new StringBuilder();
        ConsExpr curr = (ConsExpr) args[0];
        while ((curr != null) && (curr.head != null)) {
            if (!(curr.head instanceof CharExpr)) {
                throw new LispException("List in "+name+" must be all characters");
            }
            builder.append(((CharExpr)curr.head).value);
            curr = curr.tail;
        }
        return new StringExpr(builder.toString());
    }
}

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
public class StringToList extends BuiltinFunctionExpr {
    public StringToList(String name) {
        super(name, "string -> cons char");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        if (!(args[0] instanceof StringExpr)) {
            throw new LispException("First argument to "+name+" must be a string");
        }
        ConsExpr curr = null;
        String str = ((StringExpr)args[0]).value;
        for (int i=str.length()-1; i >= 0; i--) {
            curr = new ConsExpr(new CharExpr(str.charAt(i)), curr);
        }
        return curr;
    }
}

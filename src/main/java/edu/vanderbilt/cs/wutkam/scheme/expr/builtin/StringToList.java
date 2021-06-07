package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.CharExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.ConsExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.StringExpr;

/** Converts a string to a list of chars
 */
public class StringToList extends BuiltinFunctionExpr {
    public StringToList(String name) {
        super(name, "string -> cons char");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        ConsExpr curr = null;
        // Type unification ensures this will be a string
        String str = ((StringExpr)args[0]).value;

        // Build the list backwards starting with null and prepending the string
        // characters starting from the end
        for (int i=str.length()-1; i >= 0; i--) {
            curr = new ConsExpr(new CharExpr(str.charAt(i)), curr);
        }
        return curr;
    }
}

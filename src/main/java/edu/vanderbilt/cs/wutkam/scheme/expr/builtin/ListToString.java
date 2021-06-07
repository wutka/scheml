package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.CharExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.ConsExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.StringExpr;

/** Converts a list of chars to a string
 */
public class ListToString extends BuiltinFunctionExpr {
    public ListToString(String name) {
        super(name, "cons char -> string");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        StringBuilder builder = new StringBuilder();
        // Type unification ensures this will be a cons
        ConsExpr curr = (ConsExpr) args[0];

        // Loop through the list appending each char to the builder
        while ((curr != null) && (curr.head != null)) {
            // Don't need to check to see that curr.head is a CharExpr,
            // the type unification will ensure that it always is
            builder.append(((CharExpr)curr.head).value);

            curr = curr.tail;
        }
        return new StringExpr(builder.toString());
    }
}

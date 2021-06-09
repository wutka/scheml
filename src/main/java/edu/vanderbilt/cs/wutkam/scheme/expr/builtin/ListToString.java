package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.CharExpr;
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
        AbstractTypeExpr curr = (AbstractTypeExpr) args[0];

        // Loop through the list appending each char to the builder
        while (curr.constructorName.equals("Cons")) {
            // Don't need to check to see that curr.head is a CharExpr,
            // the type unification will ensure that it always is
            builder.append(((CharExpr)curr.values.get(0)).value);

            curr = (AbstractTypeExpr) curr.values.get(1);
        }
        return new StringExpr(builder.toString());
    }
}

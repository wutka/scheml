package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.CharExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheme.type.builtin.ConsTypeDecl;

/** Splits a string in to a list of strings
 */
public class Split extends BuiltinFunctionExpr {
    public Split(String name) {
        super(name, "string -> string -> cons string");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        AbstractTypeExpr curr = ConsTypeDecl.newNil();
        // Type unification ensures this will be a string
        String[] splits = ((StringExpr)args[0]).value.split(((StringExpr)args[1]).value);

        // Build the list backwards starting with null and prepending the string
        // characters starting from the end
        for (int i=splits.length-1; i >= 0; i--) {
            curr = ConsTypeDecl.newCons(new StringExpr(splits[i]), curr);
        }
        return curr;
    }
}

package edu.vanderbilt.cs.wutkam.scheml.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.CharExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;

/** Converts a string to a list of chars
 */
public class StringToList extends BuiltinFunctionExpr {
    public StringToList(String name) {
        super(name, "string -> cons char");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        AbstractTypeExpr curr = ConsTypeDecl.newNil();
        // Type unification ensures this will be a string
        String str = ((StringExpr)args[0]).value;

        // Build the list backwards starting with null and prepending the string
        // characters starting from the end
        for (int i=str.length()-1; i >= 0; i--) {
            curr = ConsTypeDecl.newCons(new CharExpr(str.charAt(i)), curr);
        }
        return curr;
    }
}

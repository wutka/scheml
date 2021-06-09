package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.IntExpr;
import edu.vanderbilt.cs.wutkam.scheme.type.builtin.ConsTypeDecl;

/** Returns a list from a to be inclusive (range 1 5) = (1 2 3 4 5)
 */
public class Range extends BuiltinFunctionExpr {
    public Range(String name) {
        super(name, "int -> int -> cons int");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        AbstractTypeExpr curr = ConsTypeDecl.newNil();
        // Build the cons list backwards starting from the nil and prepending each new value
        for (int i=((IntExpr)args[1]).value; i >= ((IntExpr)args[0]).value; i--) {
            curr = ConsTypeDecl.newCons(new IntExpr(i), curr);
        }
        return curr;
    }
}

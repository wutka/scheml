package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.array;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.ArrayExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;

/** Converts and array to a list */
public class ArrayToList extends BuiltinFunctionExpr {
    public ArrayToList(String name) {
        super(name, "array 'a -> cons 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        ArrayExpr arr = (ArrayExpr) args[0];
        AbstractTypeExpr curr = ConsTypeDecl.newNil();
        for (int i=arr.values.length-1; i >= 0; i--) {
            curr = ConsTypeDecl.newCons(arr.values[i], curr);
        }
        return curr;
    }
}

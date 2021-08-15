package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.array;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.ArrayExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.IntExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.FailException;

/** Returns the value at a particular array location */
public class ArrayRef extends BuiltinFunctionExpr {
    public ArrayRef(String name) {
        super(name, "array 'a -> int -> 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        int ref = (int) ((IntExpr)args[1]).value;
        ArrayExpr arr = (ArrayExpr) args[0];
        if (ref < 0) {
            throw new FailException("Array index < 0");
        } else if (ref >= arr.values.length) {
            throw new FailException("Array index >= size");
        } else {
            return arr.values[ref];
        }
    }
}

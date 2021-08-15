package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.array;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.ArrayExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.IntExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;

/** Returns the length of an array */
public class ArrayLen extends BuiltinFunctionExpr {
    public ArrayLen(String name) {
        super(name, "array 'a -> int");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        return new IntExpr(((ArrayExpr)args[0]).values.length);
    }
}

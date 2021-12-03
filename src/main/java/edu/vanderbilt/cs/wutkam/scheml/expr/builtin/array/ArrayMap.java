package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.array;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Applicable;
import edu.vanderbilt.cs.wutkam.scheml.expr.ArrayExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.IntExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;

import java.util.Arrays;

/** Maps a function over an array and returns a new array */
public class ArrayMap extends BuiltinFunctionExpr {
    public ArrayMap(String name) {
        super(name, "('a -> 'b) -> array 'a -> array 'b");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        Applicable f = (Applicable) args[0];
        ArrayExpr oldArray = (ArrayExpr) args[1];

        Expression[] newValues = new Expression[oldArray.values.length];

        for (int i=0; i < newValues.length; i++) {
            newValues[i] = applyLoop(f, Arrays.asList(oldArray.values[i]));
        }

        return new ArrayExpr(newValues);
    }
}

package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.array;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Applicable;
import edu.vanderbilt.cs.wutkam.scheml.expr.ArrayExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.IntExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;

import java.util.Arrays;

/** Creates a new array from a size and default value */
public class MakeArrayWithFunction extends BuiltinFunctionExpr {
    public MakeArrayWithFunction(String name) {
        super(name, "int -> (int -> 'a) -> array 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        Expression[] values = new Expression[(int)((IntExpr)args[0]).value];
        Applicable f = (Applicable) args[1];
        for (int i=0; i < values.length; i++) {
            values[i] = applyLoop(f, Arrays.asList(new IntExpr(i)));
        }
        return new ArrayExpr(values);
    }
}

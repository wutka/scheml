package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.array;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.*;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;

import java.util.ArrayList;
import java.util.List;

/** Creates a new array from a size and default value */
public class MakeArrayWithDefault extends BuiltinFunctionExpr {
    public MakeArrayWithDefault(String name) {
        super(name, "int -> 'a -> array 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        return new ArrayExpr((int)((IntExpr)args[0]).value, args[1]);
    }
}

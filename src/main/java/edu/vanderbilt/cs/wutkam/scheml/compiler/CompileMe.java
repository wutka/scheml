package edu.vanderbilt.cs.wutkam.scheml.compiler;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;

import java.util.Arrays;

public class CompileMe {
    public Expression doSomething(Expression x1, Expression x2) throws LispException {
        BuiltinFunctionExpr builtin = (BuiltinFunctionExpr) SchemlRuntime.getTopLevel().lookup("+");
        return builtin.apply(Arrays.asList(x1, x2), new Environment<>());
    }
}

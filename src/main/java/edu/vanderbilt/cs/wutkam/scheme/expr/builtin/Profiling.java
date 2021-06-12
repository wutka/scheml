package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.BoolExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.VoidExpr;
import edu.vanderbilt.cs.wutkam.scheme.runtime.SchemlRuntime;

/** Turns profiling on/off
 */
public class Profiling extends BuiltinFunctionExpr {
    public Profiling(String name) {
        super(name, "bool -> void");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        if (((BoolExpr)args[0]).value) {
            SchemlRuntime.getProfiler().enable();
        } else {
            SchemlRuntime.getProfiler().disable();
        }
        return new VoidExpr();
    }
}

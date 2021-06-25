package edu.vanderbilt.cs.wutkam.scheml.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.BoolExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.VoidExpr;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;

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

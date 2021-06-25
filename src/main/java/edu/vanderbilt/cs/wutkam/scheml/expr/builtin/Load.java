package edu.vanderbilt.cs.wutkam.scheml.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.VoidExpr;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;

/** loads a file from disk and evaluates it
 */
public class Load extends BuiltinFunctionExpr {
    public Load(String name) {
        super(name, "string -> void");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        Expression expr = args[0];
        // Type unification will ensure the expr is a string
        SchemlRuntime.getRepl().loadFile(((StringExpr)expr).value, false);
        return new VoidExpr();
    }
}

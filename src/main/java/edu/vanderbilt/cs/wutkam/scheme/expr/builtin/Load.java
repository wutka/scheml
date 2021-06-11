package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.VoidExpr;
import edu.vanderbilt.cs.wutkam.scheme.parser.Parser;
import edu.vanderbilt.cs.wutkam.scheme.repl.Repl;

import java.io.FileReader;
import java.util.List;

/** Prints a string to stdout
 */
public class Load extends BuiltinFunctionExpr {
    public Load(String name) {
        super(name, "string -> void");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        Expression expr = args[0];
        // Type unification will ensure the expr is a string
        Repl.loadFile(((StringExpr)expr).value);
        return new VoidExpr();
    }
}

package edu.vanderbilt.cs.wutkam.scheml.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;

/** Reads a list of lines from a file
 */
public class ReadLines extends BuiltinFunctionExpr {
    public ReadLines(String name) {
        super(name, "string -> cons string");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        Expression expr = args[0];
        // Type unification will ensure the expr is a string
        try {
            File file = SchemlRuntime.getRepl().getFile(((StringExpr)expr).value);
            List<String> lines = Files.readAllLines(file.toPath());
            AbstractTypeExpr curr = ConsTypeDecl.newNil();
            for (int i=lines.size()-1; i >= 0; i--) {
                curr = ConsTypeDecl.newCons(new StringExpr(lines.get(i)), curr);
            }
            return curr;
        } catch (IOException exc) {
            throw new FailException("Error reading file "+((StringExpr)expr).value+": "+exc.getMessage());
        }
    }
}

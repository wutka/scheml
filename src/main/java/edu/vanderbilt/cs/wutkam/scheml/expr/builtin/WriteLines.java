package edu.vanderbilt.cs.wutkam.scheml.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.VoidExpr;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

/** Writes a list of string as separate lines in the named file
 */
public class WriteLines extends BuiltinFunctionExpr {
    public WriteLines(String name) {
        super(name, "string -> cons string -> void");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        String filename = ((StringExpr)args[0]).value;
        AbstractTypeExpr stringList = (AbstractTypeExpr) args[1];
        try {
            File file = SchemlRuntime.getRepl().getFile(filename);
            FileWriter out = new FileWriter(file);
            while (!stringList.constructorName.equals("Nil")) {
                out.write(((StringExpr)stringList.values.get(0)).value);
                out.write("\n");
                stringList = (AbstractTypeExpr) stringList.values.get(1);
            }
            out.close();
            return new VoidExpr();
        } catch (IOException exc) {
            throw new LispException("Error writing to file "+filename+": "+exc.getMessage());
        }
    }
}

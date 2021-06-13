package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.VoidExpr;

import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

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
            FileWriter out = new FileWriter(filename);
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

package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.VoidExpr;
import edu.vanderbilt.cs.wutkam.scheme.type.builtin.ConsTypeDecl;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
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
            List<String> lines = Files.readAllLines(Paths.get(((StringExpr) expr).value));
            AbstractTypeExpr curr = ConsTypeDecl.newNil();
            for (int i=lines.size()-1; i >= 0; i--) {
                curr = ConsTypeDecl.newCons(new StringExpr(lines.get(i)), curr);
            }
            return curr;
        } catch (IOException exc) {
            throw new LispException("Error reading file "+((StringExpr)expr).value+": "+exc.getMessage());
        }
    }
}

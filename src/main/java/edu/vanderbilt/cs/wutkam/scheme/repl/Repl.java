package edu.vanderbilt.cs.wutkam.scheme.repl;

import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheme.forms.FormExpander;
import edu.vanderbilt.cs.wutkam.scheme.parser.Parser;
import edu.vanderbilt.cs.wutkam.scheme.runtime.SchemeRuntime;

import java.io.DataInputStream;
import java.io.FileReader;
import java.util.List;

public class Repl {
    public static void main(String[] args) {
        System.out.println("\n\n\nScheme-ish Repl\n\n");
        DataInputStream dataIn = new DataInputStream(System.in);
        for (;;) {
            try {
                System.out.print(">"); System.out.flush();
                String line = dataIn.readLine();
                if (line == null) System.exit(0);
                List<Expression> exprs;
                if (line.startsWith(":r ")) {
                    String filename = line.substring(3).trim();
                    FileReader in = new FileReader(filename);
                    exprs = Parser.parse(in);
                } else {
                    exprs = Parser.parse(line);
                }
                for (Expression expr: exprs) {
                    if (expr instanceof ListExpr) {
                        expr = FormExpander.expand((ListExpr) expr, true);
                    }
                    expr = expr.evaluate(SchemeRuntime.getTopLevel());
                    System.out.println(expr);
                }
            } catch (Exception exc) {
                exc.printStackTrace(System.out);
            }
        }
    }
}

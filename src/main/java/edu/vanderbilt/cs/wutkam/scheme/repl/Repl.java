package edu.vanderbilt.cs.wutkam.scheme.repl;

import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.VoidExpr;
import edu.vanderbilt.cs.wutkam.scheme.forms.FormExpander;
import edu.vanderbilt.cs.wutkam.scheme.parser.Parser;
import edu.vanderbilt.cs.wutkam.scheme.runtime.SchemeRuntime;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheme.type.UnifyException;

import java.io.DataInputStream;
import java.io.FileReader;
import java.util.List;

public class Repl {
    public static void main(String[] args) {
        System.out.println("\nScheme-ish Repl");
        DataInputStream dataIn = new DataInputStream(System.in);
        for (;;) {
            try {
                System.out.print(">");
                System.out.flush();
                String line = dataIn.readLine();
                if (line == null) System.exit(0);
                List<Expression> exprs;
                boolean displayType = false;
                if (line.startsWith(":r ")) {
                    String filename = line.substring(3).trim();
                    FileReader in = new FileReader(filename);
                    exprs = Parser.parse(in);
                } else if (line.startsWith(":t ")) {
                    exprs = Parser.parse(line.substring(3));
                    displayType = true;
                } else {
                    exprs = Parser.parseWithPrompt(line, dataIn);
                }
                for (Expression expr : exprs) {
                    if (expr instanceof ListExpr) {
                        expr = FormExpander.expand((ListExpr) expr, true);
                    }
                    TypeRef exprType = new TypeRef();
                    expr.unify(exprType, SchemeRuntime.getUnifyTopLevel());
                    expr = expr.evaluate(SchemeRuntime.getTopLevel(), false);
                    if (displayType) {
                        TypeRef resultType = new TypeRef();
                        try {
                            expr.unify(resultType, SchemeRuntime.getUnifyTopLevel());
                            System.out.print(expr);
                            System.out.print(" : ");
                            System.out.println(resultType.getType());
                        } catch (UnifyException exc) {
                            System.out.println("Error unifying expression: " + expr + "\n" + exc.getMessage());
                        }
                    } else {
                        if (!(expr instanceof VoidExpr)) {
                            System.out.println(expr);
                        }
                    }
                }
            } catch (UnifyException exc) {
                System.out.println("Unification error:\n" + exc.getMessage());
            } catch (StackOverflowError exc) {
                System.out.println("Stack overflow");
            } catch (Exception exc) {
                exc.printStackTrace(System.out);
            }
        }
    }
}

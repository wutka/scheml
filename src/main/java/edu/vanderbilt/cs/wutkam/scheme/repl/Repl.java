package edu.vanderbilt.cs.wutkam.scheme.repl;

import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.VoidExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.builtin.FailException;
import edu.vanderbilt.cs.wutkam.scheme.forms.FormExpander;
import edu.vanderbilt.cs.wutkam.scheme.parser.Parser;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.runtime.SchemeRuntime;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheme.type.UnifyException;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.util.List;

public class Repl {
    public static void main(String[] args) {
        System.out.println("\nScheml Repl");

        BufferedReader dataIn = new BufferedReader(new InputStreamReader(System.in));
        for (;;) {
            try {
                // Print a prompt
                System.out.print(">");
                System.out.flush();

                // Read a line
                String line = dataIn.readLine();
                if (line == null) System.exit(0);


                // Unless the line starts with :t, don't display the type of the result
                boolean displayType = false;

                List<Expression> exprs;
                if (line.startsWith(":r ")) {
                    // load a file, where the filename comes after the ":r "
                    String filename = line.substring(3).trim();
                    FileReader in = new FileReader(filename);
                    exprs = Parser.parse(in);
                } else if (line.startsWith(":t ")) {
                    // A :t means display the type of the expression, parse the rest
                    exprs = Parser.parse(line.substring(3));
                    displayType = true;
                } else {
                    // Otherwise just parse the line, but if the line is incomplete (as when pasting code
                    // into the repl), let the parser prompt for more data, but right now just make the
                    // prompt empty, characters get in the way
                    exprs = Parser.parseWithPrompt(line, "", dataIn);
                }
                for (Expression expr : exprs) {
                    // For each expression parsed, expand if necessary
                    if (expr instanceof ListExpr) {
                        expr = FormExpander.expand((ListExpr) expr, true);
                    }

                    // Do a type check - this is necessary since the interpreter assumes that types have
                    // been checked and it makes a lot of assumptions when doing type casts
                    TypeRef exprType = new TypeRef();
                    expr.unify(exprType, new Environment<>());

                    // Evaluate the expression
                    expr = expr.evaluate(new Environment<>(), false);

                    for (String warning: SchemeRuntime.getWarnings()) {
                        System.out.println(warning);
                    }

                    if (displayType) {
                        TypeRef resultType = new TypeRef();
                        // If we also are displaying the type of the result, get the type and print it
                        try {
                            expr.unify(resultType, new Environment<>());
                            System.out.print(expr);
                            System.out.print(" : ");
                            System.out.println(resultType.getType());
                        } catch (UnifyException exc) {
                            System.out.println("Error unifying expression: " + expr + "\n" + exc.getMessage());
                        }
                    } else {
                        // Don't bother printing a void result, which is returned by functions like print
                        if (!(expr instanceof VoidExpr)) {
                            System.out.println(expr);
                        }
                    }
                }
            } catch (UnifyException exc) {
                System.out.println("Unification error:\n" + exc.getMessage());
            } catch (FailException exc) {
                System.out.println("Fail: "+exc.getMessage());
            } catch (StackOverflowError exc) {
                System.out.println("Stack overflow");
            } catch (Exception exc) {
                exc.printStackTrace(System.out);
            }
        }
    }
}

package edu.vanderbilt.cs.wutkam.scheme.repl;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.VoidExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.builtin.FailException;
import edu.vanderbilt.cs.wutkam.scheme.forms.FormExpander;
import edu.vanderbilt.cs.wutkam.scheme.parser.Parser;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.runtime.SchemlRuntime;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheme.type.UnifyException;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.List;

/** Represents the runtime Read-Eval-Print loop, but with hooks to allow other parts of the
 * system to interact with the repl
 */
public class Repl {
    protected BufferedReader dataIn;

    protected Environment<Expression> exprEnvironment;
    protected Environment<TypeRef> typeEnvironment;
    protected File currentDir;

    public Repl() {
        dataIn = new BufferedReader(new InputStreamReader(System.in));
        this.exprEnvironment = new Environment<>();
        this.typeEnvironment = new Environment<>();
        this.currentDir = new File(".");
    }

    public static void main(String [] args) {
        Repl repl = new Repl();
        SchemlRuntime.repl = repl;
        repl.run(args);
    }

    public File getFile(String filename) {
        if (filename.startsWith("/")) return new File(filename);
        return new File(currentDir, filename);
    }

    public void run(String[] args) {
        System.out.println("\nScheml Repl");

        for (String arg: args) {
            loadFile(arg, false);
        }
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
                if (line.startsWith(":cd ")) {
                    String dir = line.substring(4).trim();
                    File newDir = getFile(dir);
                    if (newDir.exists() && newDir.isDirectory()) {
                        currentDir = newDir;
                    }
                    continue;
                } else if (line.equals(":pwd")) {
                    System.out.println(currentDir.getCanonicalPath());
                    continue;
                } else if (line.startsWith(":!")) {
                    String commandLine = line.substring(2).trim();
                    Process p = Runtime.getRuntime().exec(commandLine, null, currentDir);
                    new Thread(new SyncPipe(p.getErrorStream(), System.err)).start();
                    new Thread(new SyncPipe(p.getInputStream(), System.out)).start();
                    p.getOutputStream().close();
                    int returnCode = p.waitFor();
                    System.out.flush();
                    continue;
                } else if (line.startsWith(":r ")) {
                    // load a file, where the filename comes after the ":r "
                    String filename = line.substring(3).trim();
                    loadFile(filename, true);
                    continue;
                } else if (line.startsWith(":t ")) {
                    // A :t means display the type of the expression, parse the rest
                    exprs = Parser.parse(line.substring(3), false);
                    displayType = true;
                } else {
                    // Otherwise just parse the line, but if the line is incomplete (as when pasting code
                    // into the repl), let the parser prompt for more data, but right now just make the
                    // prompt empty, characters get in the way
                    exprs = Parser.parseWithPrompt(line, "", dataIn, false);
                }

                executeExpressions(exprs, displayType);
            } catch (Exception exc) {
                exc.printStackTrace(System.out);
            }
        }
    }

    /** Since the (input) function wants to read a line, let it use the same stream the repl does */
    public String readLine() throws LispException {
        try {
            return dataIn.readLine();
        } catch (IOException exc) {
            throw new LispException("Error reading input line: "+exc.getMessage());
        }
    }

    /** Expose the loading operation performed by :r so that the (load) function can use it too */
    public void loadFile(String filename, boolean display) {
        try {
            File file = getFile(filename);
            FileReader in = new FileReader(file);
            List<Expression> exprs = Parser.parse(in, display);
            executeExpressions(exprs, false);
        } catch (IOException exc) {
            System.out.println("Exception loading file: "+exc.getMessage());
        } catch (LispException exc) {
            System.out.println("Exception loading file:\n");
            exc.printStackTrace(System.out);
        }
    }

    /** Executes a list of expressions */
    public void executeExpressions(List<Expression> exprs, boolean displayType) {
        try {
            for (Expression expr : exprs) {
                // For each expression parsed, expand if necessary
                if (expr instanceof ListExpr) {
                    expr = FormExpander.expand((ListExpr) expr, true);
                }

                // Do a type check - this is necessary since the interpreter assumes that types have
                // been checked and it makes a lot of assumptions when doing type casts
                TypeRef exprType = new TypeRef();
                expr.unify(exprType, typeEnvironment);

                for (String warning: SchemlRuntime.getWarnings()) {
                    System.out.println(warning);
                }

                // Evaluate the expression
                expr = expr.evaluate(exprEnvironment, false);

                for (String warning: SchemlRuntime.getWarnings()) {
                    System.out.println(warning);
                }

                if (displayType) {
                    TypeRef resultType = new TypeRef();
                    // If we also are displaying the type of the result, get the type and print it
                    try {
                        expr.unify(resultType, typeEnvironment);
                        System.out.print(expr);
                        System.out.print(" : ");
                        System.out.println(resultType.getType());
                    } catch (UnifyException exc) {
                        System.out.println("Error unifying expression: " + expr + "\n" + exc.getMessage());
                    }
                    for (String warning: SchemlRuntime.getWarnings()) {
                        System.out.println(warning);
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
            System.out.println("Fail: " + exc.getMessage());
        } catch (LispException exc) {
            System.out.println("Lisp Exception: " + exc.getMessage());
        } catch (StackOverflowError exc) {
            System.out.println("Stack overflow");
//            exc.printStackTrace();
        } catch (Exception exc) {
            exc.printStackTrace(System.out);
        }
        for (String warning: SchemlRuntime.getWarnings()) {
            System.out.println(warning);
        }
    }

    static class SyncPipe implements Runnable
    {
        public SyncPipe(InputStream istrm, OutputStream ostrm) {
            istrm_ = istrm;
            ostrm_ = ostrm;
        }
        public void run() {
            try
            {
                final byte[] buffer = new byte[1024];
                for (int length = 0; (length = istrm_.read(buffer)) != -1; )
                {
                    ostrm_.write(buffer, 0, length);
                }
            }
            catch (Exception e)
            {
            }
        }
        private final OutputStream ostrm_;
        private final InputStream istrm_;
    }
}

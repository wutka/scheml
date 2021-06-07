package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.StringType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheme.type.UnifyException;
import edu.vanderbilt.cs.wutkam.scheme.type.VoidType;

import java.util.List;

/** Provides a wrapper for Java System.out.printf/String.format allowing for multiple, varied arguments
 * The format strings are those specified by java.util.Formatter.
 */
public class PrintfExpr implements Expression {
    Expression formatExpr;
    boolean returnString;
    List<Expression> parameters;

    public PrintfExpr(boolean returnString, Expression formatExpr, List<Expression> parameters) {
        this.returnString = returnString;
        this.formatExpr = formatExpr;
        this.parameters = parameters;

    }
    
    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        Object[] args = new Object[parameters.size()];
        for (int i=0; i < args.length; i++) {
            args[i] = parameters.get(i).evaluate(env, false).toJavaValue();
        }

        // Evaluate the format string (it might be a constant, or it might be computed)
        // We can be sure it evaluates to a string because it would have failed unification if not
        String formatString = ((StringExpr) formatExpr.evaluate(env, false)).value;

        if (returnString) {
            // If this is an sprintf, we return a string
            return new StringExpr(String.format(formatString, args));
        } else {
            // Otherwise we print to stdout
            System.out.printf(formatString, args);
            System.out.flush();
            return new VoidExpr();
        }
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        // Make sure that the format string is really a string
        TypeRef formatStringType = new TypeRef(StringType.TYPE);
        try {
            formatExpr.unify(formatStringType, env);
        } catch (UnifyException exc) {
            throw UnifyException.addCause("printf format string should be a string expression", exc);
        }

        // We don't unify on the parameters. Theoretically we could, by examining the format string, but that
        // is left as a future exercise.
        
        // This expression returns a string if returnString is true, otherwise a void
        if (returnString) {
            typeRef.unify(new TypeRef(StringType.TYPE));
        } else {
            typeRef.unify(new TypeRef(VoidType.TYPE));
        }
    }
}

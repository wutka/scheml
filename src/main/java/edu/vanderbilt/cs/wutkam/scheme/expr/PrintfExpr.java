package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.StringType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheme.type.UnifyException;
import edu.vanderbilt.cs.wutkam.scheme.type.VoidType;

import java.util.ArrayList;
import java.util.List;

/** Provides a wrapper for Java System.out.printf/String.format allowing for multiple, varied arguments
 * The format strings are those specified by java.util.Formatter.
 */
public class PrintfExpr implements Expression {
    String formatString;
    boolean returnString;
    List<Expression> parameters;

    public PrintfExpr(boolean returnString, String formatString, List<Expression> parameters) {
        this.returnString = returnString;
        this.formatString = formatString;
        this.parameters = parameters;

    }
    
    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        Object[] args = new Object[parameters.size()];
        for (int i=0; i < args.length; i++) {
            args[i] = parameters.get(i).evaluate(env, false).toJavaValue();
        }

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

        // We don't unify on the parameters. Theoretically we could, by examining the format string, but that
        // is left as a future exercise.
        
        // This expression returns a string if returnString is true, otherwise a void
        if (returnString) {
            typeRef.unify(new TypeRef(StringType.TYPE));
        } else {
            typeRef.unify(new TypeRef(VoidType.TYPE));
        }
    }

    static List<StringFormatTypeRef> parseFormatString(String formatString) {
        List<StringFormatTypeRef> formatTypeRefs = new ArrayList<>();

        int pos = 0;
        int length = formatString.length();
        while (pos < length) {
            char ch = formatString.charAt(pos++);
            if (ch != '%') continue;

            if (pos >= length) return formatTypeRefs;

            ch = formatString.charAt(pos++);
            int argPosition = -1;
            int width = -1;
            int precision = -1;
            if (Character.isDigit(ch)) {
                int num = ch - '0';
                while ((pos < length) && Character.isDigit(formatString.charAt(pos))) {
                    ch = formatString.charAt(pos++);
                    num = 10 * num + ch - '0';
                }
                if (pos >= length) return formatTypeRefs;
                if (formatString.charAt(pos) == '$') {
                    argPosition = num;
                } else if (formatString.charAt(pos) == '.') {
                    width = num;
                    pos++;
                    if (pos >= length) return formatTypeRefs;
                    num = 0
                    while ((pos < length) && Character.isDigit(formatString.charAt(pos))) {
                        ch = formatString.charAt(pos++);
                        num = num * 10 + ch - '0';
                    }
                }
            }
        }
    }
    static class StringFormatTypeRef {
        public TypeRef typeRef;
        public int argNumber;

        public StringFormatTypeRef(TypeRef typeRef, int argNumber) {
            this.typeRef = typeRef;
            this.argNumber = argNumber;
        }
    }
}

package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/** Provides a wrapper for Java System.out.printf/String.format allowing for multiple, varied arguments
 * The format strings are those specified by java.util.Formatter. Type checking is performed by parsing
 * the format string and matching against argument types.
 */
public class PrintfExpr implements Expression {
    String formatString;
    boolean returnString;
    List<Expression> parameters;
    List<StringFormatTypeRef> formatStringTypes;

    public PrintfExpr(boolean returnString, String formatString, List<Expression> parameters)
        throws LispException {
        this.returnString = returnString;
        this.formatString = formatString;
        this.formatStringTypes = parseFormatString(formatString);
        this.parameters = parameters;
    }
    
    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {

        // Evaluate the parameters
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

    protected static String escapeString(String str) {
        return "\""+str.replace("\\","\\\\").
                replace("\n", "\\n").
                replace("\r","\\r").
                replace("\t","\\t")+"\"";
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        // initialize the flags to make sure each arg is accounted for in the format string
        boolean[] unifiedArg = new boolean[parameters.size()];
        for (int i=0; i < unifiedArg.length; i++) unifiedArg[i] = false;

        // Unify on the type parameters
        for (StringFormatTypeRef formatTypeRef: formatStringTypes) {
            if ((formatTypeRef.argNumber < 0) || (formatTypeRef.argNumber >= parameters.size())) {
                throw new LispException("Format string refers to argument number "+(formatTypeRef.argNumber+1)+
                        " but there are only "+parameters.size()+" parameters");
            }
            TypeRef argType = formatTypeRef.typeRef.copy(new HashMap<>());
            try {
                // Unify the parameter with what is required by the format type
                parameters.get(formatTypeRef.argNumber).unify(argType, env);

                // Mark this arg as having been unified
                unifiedArg[formatTypeRef.argNumber] = true;
            } catch (UnifyException exc) {
                throw UnifyException.addCause("Error unifying arg "+(formatTypeRef.argNumber+1)+
                        " with type "+argType.getType()+" in format string "+escapeString(formatString), exc);
            }
        }

        // Make sure all the args have been unified
        for (int i=0; i < unifiedArg.length; i++) {
            if (!unifiedArg[i]) {
                throw new UnifyException("Format string arg "+(i+1)+" is not referenced in format string "+
                        escapeString(formatString));
            }
        }

        // This expression returns a string if returnString is true, otherwise a void
        if (returnString) {
            typeRef.unify(new TypeRef(StringType.TYPE));
        } else {
            typeRef.unify(new TypeRef(VoidType.TYPE));
        }
    }

    @Override
    public Expression toScheml() {
        List<Expression> scheml = new ArrayList<>();
        scheml.add(new SymbolLiteralExpr(returnString ? "sprintf" : "printf"));
        scheml.add(new StringExpr(formatString));
        for (Expression param: parameters) {
            scheml.add(param.toScheml());
        }
        return new ListExpr(scheml);
    }
    // Define a regex to match the java formatter format string, ignoring the date formats
    static final String formatRegex = "%(?<argPos>[<0-9]+[$])?(?<argLastPos><)?(?<flags>[-#+ 0,(]*)(?<width>[0-9]+)?(?<precision>[.][0-9]+)?(?<conversion>.)";
    static final Pattern formatPattern = Pattern.compile(formatRegex);

    public static List<StringFormatTypeRef> parseFormatString(String formatString)
        throws LispException {

        List<StringFormatTypeRef> formatTypes = new ArrayList<>();

        // Parse the format
        Matcher matcher = formatPattern.matcher(formatString);
        int positionalArgNumber = 0;
        int lastArgNumber = 0;

        // For each format string found...
        while (matcher.find()) {
            // Get the parameters we are interested in
            String argPos = matcher.group("argPos");
            String argLastPos = matcher.group("argLastPos");
            String conversion = matcher.group("conversion");

            int argNumber;

            if ((argPos != null) && (argPos.length() > 1)) {
                // If the argPos was specified, strip off the $ at the end and parse it as an int
                argPos = argPos.substring(0, argPos.length() - 1);
                try {
                    // Subtract 1 since the format position numbers start from 1
                    argNumber = Integer.parseInt(argPos) - 1;
                } catch (NumberFormatException exc) {
                    throw new LispException("Unable to parse arg number " + argPos + " in format string " +
                            escapeString(formatString));
                }
                // Note the last arg number used
                lastArgNumber = argNumber;
            } else if ((argLastPos != null) && (argLastPos.length() > 0)) {
                // If the argLastPos (<) was specified, use lastArgNumber
                argNumber = lastArgNumber;
            } else {
                // Otherwise use the positional arg count
                lastArgNumber = positionalArgNumber;
                argNumber = positionalArgNumber++;
            }

            // If the conversion is a % it's not a conversion
            if (conversion.equals("%")) continue;

            switch (conversion) {
                case "B", "H", "S", "b", "h", "s":
                    formatTypes.add(new StringFormatTypeRef(new TypeRef(), argNumber));
                    break;
                case "C", "c":
                    formatTypes.add(new StringFormatTypeRef(new TypeRef(CharType.TYPE), argNumber));
                    break;
                case "d", "o", "x", "X":
                    formatTypes.add(new StringFormatTypeRef(new TypeRef(IntType.TYPE), argNumber));
                    break;
                case "e", "E", "f", "g", "G", "a", "A":
                    formatTypes.add(new StringFormatTypeRef(new TypeRef(DoubleType.TYPE), argNumber));
                    break;
                default:
                    // Don't do anything for conversions we don't recognize
                    break;
            }
        }

        return formatTypes;
    }

    public static class StringFormatTypeRef {
        public TypeRef typeRef;
        public int argNumber;

        public StringFormatTypeRef(TypeRef typeRef, int argNumber) {
            this.typeRef = typeRef;
            this.argNumber = argNumber;
        }
    }
}

package edu.vanderbilt.cs.wutkam.scheml.runtime;

import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinInitializer;
import edu.vanderbilt.cs.wutkam.scheml.parser.Parser;
import edu.vanderbilt.cs.wutkam.scheml.repl.Repl;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;

import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

/** A holder for some of the symbol tables used by the runtime
 */
public class SchemlRuntime {
    /** The table of global functions and variables */
    protected static final Environment<Expression> topLevelEnvironment = new Environment<Expression>();

    /** The table of types of global functions and variables */
    protected static final Environment<TypeRef> topLevelUnifyEnvironment = new Environment<TypeRef>();

    /** The registry of abstract types defined with the (type) form */
    protected static final TypeRegistry typeRegistry = new TypeRegistry();

    protected static final List<String> warnings = new ArrayList<>();

    protected static final Profiler profiler = new Profiler();

    /** Initialize the builtin functions */
    static {
        BuiltinInitializer.initializeBuiltins(topLevelEnvironment, topLevelUnifyEnvironment);
    }

    public static Environment<Expression> getTopLevel() { return topLevelEnvironment; }
    public static Environment<TypeRef> getUnifyTopLevel() { return topLevelUnifyEnvironment; }
    public static TypeRegistry getTypeRegistry() { return typeRegistry; }

    public static void warn(String warning) {
        warnings.add(warning);
    }

    public static String[] getWarnings() {
        String[] result = warnings.toArray(new String[warnings.size()]);
        warnings.clear();
        return result;
    }

    public static Repl repl;

    public static Repl getRepl() {
        return repl;
    }

    public static Profiler getProfiler() { return profiler; }

    public static void initializeStdlib() {
        try {
            List<Expression> exprs = Parser.parse(new InputStreamReader(
                    SchemlRuntime.class.getClassLoader().getResourceAsStream(
                            "edu/vanderbilt/cs/wutkam/scheml/stdlib.scm")), false);
            repl.executeExpressions(exprs, false);
        } catch (Exception exc) {
            exc.printStackTrace();
        }

    }
}

package edu.vanderbilt.cs.wutkam.scheml.runtime;

import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinInitializer;
import edu.vanderbilt.cs.wutkam.scheml.forms.Macro;
import edu.vanderbilt.cs.wutkam.scheml.parser.Parser;
import edu.vanderbilt.cs.wutkam.scheml.repl.Repl;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;

import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/** A holder for some of the symbol tables used by the runtime
 */
public class SchemlRuntime {
    /** The table of global functions and variables */
    protected static Environment<Expression> topLevelEnvironment = new Environment<>();

    /** The table of types of global functions and variables */
    protected static Environment<TypeRef> topLevelUnifyEnvironment = new Environment<>();

    /** The registry of abstract types defined with the (type) form */
    protected static TypeRegistry typeRegistry = new TypeRegistry();

    protected static List<String> warnings = new ArrayList<>();

    protected static Profiler profiler = new Profiler();

    protected static Map<String,Macro> macroTable = new HashMap<>();

    /** Initialize the builtin functions */
    static {
        typeRegistry.initializeBuiltinTypes();
        BuiltinInitializer.initializeBuiltins(topLevelEnvironment, topLevelUnifyEnvironment);
    }

    public static Environment<Expression> getTopLevel() { return topLevelEnvironment; }
    public static Environment<TypeRef> getUnifyTopLevel() { return topLevelUnifyEnvironment; }
    public static TypeRegistry getTypeRegistry() { return typeRegistry; }
    public static Map<String,Macro> getMacroTable() { return macroTable; }

    public static void warn(String warning) {
        warnings.add(warning);
    }

    public static String[] getWarnings() {
        String[] result = warnings.toArray(new String[warnings.size()]);
        return result;
    }

    public static void clearWarnings() {
        warnings.clear();
    }

    public static Repl repl;

    public static Repl getRepl() {
        return repl;
    }

    public static Profiler getProfiler() { return profiler; }

    public static void initializeStdlib() {
        try {
            boolean displayExpressions = repl.getDisplayExpressions();
            repl.setDisplayExpressions(false);
            List<Expression> exprs = Parser.parse(new InputStreamReader(
                    Thread.currentThread().getContextClassLoader().getResourceAsStream(
                            "edu/vanderbilt/cs/wutkam/scheml/stdlib.scm")), false);
            repl.setDisplayExpressions(displayExpressions);
            repl.executeExpressions(exprs, false);
        } catch (Exception exc) {
            exc.printStackTrace();
        }

    }

    public static void reset() {
        topLevelEnvironment = new Environment<>();
        topLevelUnifyEnvironment = new Environment<>();
        typeRegistry = new TypeRegistry();
        typeRegistry.initializeBuiltinTypes();
        macroTable = new HashMap<>();
        warnings = new ArrayList<>();
        profiler = new Profiler();
        repl = new Repl();

        BuiltinInitializer.initializeBuiltins(topLevelEnvironment, topLevelUnifyEnvironment);
        initializeStdlib();
    }
}

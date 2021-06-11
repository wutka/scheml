package edu.vanderbilt.cs.wutkam.scheme.runtime;

import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.builtin.BuiltinInitializer;
import edu.vanderbilt.cs.wutkam.scheme.repl.Repl;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

import java.util.ArrayList;
import java.util.List;

/** A holder for some of the symbol tables used by the runtime
 */
public class SchemeRuntime {
    /** The table of global functions and variables */
    protected static final Environment<Expression> topLevelEnvironment = new Environment<Expression>();

    /** The table of types of global functions and variables */
    protected static final Environment<TypeRef> topLevelUnifyEnvironment = new Environment<TypeRef>();

    /** The registry of abstract types defined with the (type) form */
    protected static final TypeRegistry typeRegistry = new TypeRegistry();

    protected static final List<String> warnings = new ArrayList<>();

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
}

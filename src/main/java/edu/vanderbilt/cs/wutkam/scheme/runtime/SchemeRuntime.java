package edu.vanderbilt.cs.wutkam.scheme.runtime;

import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.builtin.BuiltinInitializer;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

/** A holder for some of the symbol tables used by the runtime
 */
public class SchemeRuntime {
    /** The table of global functions and variables */
    protected static final Environment<Expression> topLevelEnvironment = new Environment<Expression>();

    /** The table of types of global functions and variables */
    protected static final Environment<TypeRef> topLevelUnifyEnvironment = new Environment<TypeRef>();

    /** The registry of abstract types defined with the (type) form */
    protected static final TypeRegistry typeRegistry = new TypeRegistry();

    /** Initialize the builtin functions */
    static {
        BuiltinInitializer.initializeBuiltins(topLevelEnvironment, topLevelUnifyEnvironment);
    }

    public static Environment<Expression> getTopLevel() { return topLevelEnvironment; }
    public static Environment<TypeRef> getUnifyTopLevel() { return topLevelUnifyEnvironment; }
    public static TypeRegistry getTypeRegistry() { return typeRegistry; }
}

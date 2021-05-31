package edu.vanderbilt.cs.wutkam.scheme.runtime;

import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.FunctionExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.builtin.BuiltinInitializer;
import edu.vanderbilt.cs.wutkam.scheme.type.FunctionType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/25/21
 * Time: 9:13 AM
 */
public class SchemeRuntime {
    protected static final Environment<Expression> topLevelEnvironment = new Environment<Expression>();
    protected static final Environment<TypeRef> topLevelUnifyEnvironment = new Environment<TypeRef>();

    static {
        BuiltinInitializer.initializeBuiltins(topLevelEnvironment);
    }

    public static Environment<Expression> getTopLevel() { return topLevelEnvironment; }
    public static Environment<TypeRef> getUnifyTopLevel() { return topLevelUnifyEnvironment; }

}

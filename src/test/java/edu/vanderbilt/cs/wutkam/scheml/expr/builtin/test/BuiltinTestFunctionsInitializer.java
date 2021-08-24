package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.test;

import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.FunctionType;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;

public class BuiltinTestFunctionsInitializer {
    protected static BuiltinFunctionExpr[] builtins = new BuiltinFunctionExpr[] {
        new AssertEquals("assert-equals"),
        new AssertNotEquals("assert-not-equals"),
        new AssertTrue("assert-true"),
        new AssertFalse("assert-false"),
        new AssertWarning("assert-warning"),
        new AssertNoWarning("assert-no-warning"),
    };


    public static void initializeBuiltins(Environment<Expression> exprEnv, Environment<TypeRef> typeEnv) {
        for (BuiltinFunctionExpr builtin: builtins) {
            exprEnv.define(builtin.name, builtin);
            typeEnv.define(builtin.name, new TypeRef(new FunctionType(builtin.paramTypes.length, builtin.paramTypes,
                    builtin.returnType)));
        }
    }
}

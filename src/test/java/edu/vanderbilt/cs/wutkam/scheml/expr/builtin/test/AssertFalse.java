package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.test;

import edu.vanderbilt.cs.wutkam.scheml.expr.BoolExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.VoidExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;

import static org.junit.jupiter.api.Assertions.assertFalse;

/** Asserts that an expression is true
 */
public class AssertFalse extends BuiltinFunctionExpr {
    public AssertFalse(String name) {
        super(name, "bool -> string -> void");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) {
        assertFalse(((BoolExpr)args[0]).value, ((StringExpr)args[1]).value);
        return new VoidExpr();
    }
}

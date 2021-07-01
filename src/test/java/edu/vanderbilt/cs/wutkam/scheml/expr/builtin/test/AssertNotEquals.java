package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.test;

import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.VoidExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;

import static org.junit.jupiter.api.Assertions.assertNotEquals;

/** Asserts that two expressions are not equal
 */
public class AssertNotEquals extends BuiltinFunctionExpr {
    public AssertNotEquals(String name) {
        super(name, "'a -> 'b -> string -> void");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) {
        assertNotEquals(args[0], args[1], ((StringExpr)args[2]).value);
        return new VoidExpr();
    }
}

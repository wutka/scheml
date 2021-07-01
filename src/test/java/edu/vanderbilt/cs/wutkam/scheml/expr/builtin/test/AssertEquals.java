package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.test;

import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.VoidExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;

import static org.junit.jupiter.api.Assertions.assertEquals;

/** Asserts that two expressions are equal
 */
public class AssertEquals extends BuiltinFunctionExpr {
    public AssertEquals(String name) {
        super(name, "'a -> 'b -> string -> void");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) {
        assertEquals(args[0], args[1], ((StringExpr)args[2]).value);
        return new VoidExpr();
    }
}

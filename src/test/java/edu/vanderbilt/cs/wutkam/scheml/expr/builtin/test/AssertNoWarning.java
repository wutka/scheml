package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.test;

import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.VoidExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;

import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertFalse;

/** Asserts that a particular warning does not currently exist in the runtime
 */
public class AssertNoWarning extends BuiltinFunctionExpr {
    public AssertNoWarning(String name) {
        super(name, "string -> string -> void");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) {
        String warningRegex = ((StringExpr)args[0]).value;
        String message = ((StringExpr)args[1]).value;

        Pattern patt = Pattern.compile(warningRegex);
        boolean found = false;
        for (String warning: SchemlRuntime.getWarnings()) {
            if (patt.matcher(warning).find()) {
                found = true;
                break;
            }
        }
        assertFalse(found, message);
        return new VoidExpr();
    }
}

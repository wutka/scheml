package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.test;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.BoolExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;

/** Returns the type of an expression as a string
 */
public class TypeOf extends BuiltinFunctionExpr {
    public TypeOf(String name) {
        super(name, "'a -> string");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // Create a type ref to hold the type of the expression
        TypeRef theType = new TypeRef();

        // Unify the expression with the type ref
        args[0].unify(theType, new Environment<>());

        // Convert the type ref to a string
        return new StringExpr(theType.getType().toString());
    }
}

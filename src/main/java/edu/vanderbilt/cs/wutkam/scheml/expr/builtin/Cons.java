package edu.vanderbilt.cs.wutkam.scheml.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;

/** A function to create an instance of ConsExpr
 */
public class Cons extends BuiltinFunctionExpr {
    public Cons(String name) {
        super(name, "'a -> cons 'a -> cons 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // We don't don't check the types here, the type unification will take
        // care of that because of the function signature
        return ConsTypeDecl.newCons(args[0], args[1]);
    }
}

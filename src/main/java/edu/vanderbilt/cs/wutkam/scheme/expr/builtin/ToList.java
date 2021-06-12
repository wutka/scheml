package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.type.builtin.ConsTypeDecl;

/** A function to create an instance of ConsExpr
 */
public class ToList extends BuiltinFunctionExpr {
    public ToList(String name) {
        super(name, "'a -> cons 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // We don't don't check the types here, the type unification will take
        // care of that because of the function signature
        return ConsTypeDecl.newCons(args[0], ConsTypeDecl.newNil());
    }
}

package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;

/** Represents a variable assignment in a statement form, which behaves just like the binding
 * in a let expression
 */
public class AssignExpr implements Expression {
    LetExpr.Declaration declaration;

    public AssignExpr(LetExpr.Declaration declaration) {
        this.declaration = declaration;
    }

    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        declaration.define(LetExpr.LET_FORM, env, env);
        return new VoidExpr();
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        declaration.unify(LetExpr.LET_FORM, env, env);
    }
}

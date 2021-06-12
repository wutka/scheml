package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

/**
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

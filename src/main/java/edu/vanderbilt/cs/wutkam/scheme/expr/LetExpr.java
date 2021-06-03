package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheme.type.UnifyException;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/26/21
 * Time: 3:33 PM
 */
public class LetExpr implements Expression {

    public static final int LET_FORM = 0;
    public static final int LET_STAR_FORM = 1;
    public static final int LET_REC_FORM = 2;

    List<Declaration> declarations;
    int letType;
    List<Expression> body;

    public LetExpr(List<Declaration> declarations, int letType, List<Expression> body) {
        this.declarations = declarations;
        this.letType = letType;
        this.body = body;
    }
    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        Environment<Expression> letEnv = new Environment<>(env);
        for (Declaration dec: declarations) {
            if ((letType == LET_STAR_FORM) || (letType == LET_REC_FORM)) {
                if (letType == LET_REC_FORM) {
                    // Put the expression value into the environment before evaluating so that
                    // a let can contain a recursive function
                    letEnv.define(dec.name, dec.value);
                }
                // Both let* and letrec put the value into the environment so it is immediately visible to
                // other let definitions
                letEnv.define(dec.name, dec.value.evaluate(letEnv, false));
            } else {
                letEnv.define(dec.name, dec.value.evaluate(env, false));
            }
        }
        Expression last = null;
        for (int i=0; i < body.size(); i++) {
            Expression expr = body.get(i);
            if (inTailPosition && (i == body.size() - 1)) {
                // When creating a tail call expression, the last environment
                // is chopped off, because this is usually the function environment.
                // Since let also creates a new environment, create a dummy that can
                // be chopped off.
                last = expr.evaluate(new Environment<>(letEnv), true);

            } else {
                last = expr.evaluate(letEnv, inTailPosition && (i == body.size() - 1));
            }
        }
        return last;
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        Environment<TypeRef> letEnv = new Environment<>(env);
        for (Declaration decl: declarations) {
            TypeRef declTypeRef = new TypeRef();
            try {
                if ((letType == LET_STAR_FORM) || (letType == LET_REC_FORM)) {
                    // Put a typeref into the environment before unifying with the expression
                    // to allow the expression to refer to decl.name
                    letEnv.define(decl.name, declTypeRef);
                    decl.value.unify(declTypeRef, letEnv);
                } else {
                    decl.value.unify(declTypeRef, env);
                }
            } catch (UnifyException exc) {
                throw UnifyException.addCause("Error unifying let expression "+decl.name, exc);
            }
            letEnv.define(decl.name, declTypeRef);
        }
        TypeRef last = null;
        for (Expression expr: body) {
            TypeRef exprType = new TypeRef();
            try {
                expr.unify(exprType, letEnv);
            } catch (UnifyException exc) {
                throw UnifyException.addCause("Error unifying let expression value", exc);
            }
            last = exprType;
        }
        typeRef.unify(last);
    }
    public static class Declaration {
        public String name;
        public Expression value;

        public Declaration(String name, Expression value) {
            this.name = name;
            this.value = value;
        }
    }
}

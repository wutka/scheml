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
    List<Declaration> declarations;
    boolean isLetStar;
    List<Expression> body;

    public LetExpr(List<Declaration> declarations, boolean isLetStar, List<Expression> body) {
        this.declarations = declarations;
        this.isLetStar = isLetStar;
        this.body = body;
    }
    @Override
    public Expression evaluate(Environment<Expression> env) throws LispException {
        Environment<Expression> letEnv = new Environment<>(env);
        for (Declaration dec: declarations) {
            if (isLetStar) {
                letEnv.define(dec.name, dec.value.evaluate(letEnv));
            } else {
                letEnv.define(dec.name, dec.value.evaluate(env));
            }
        }
        Expression last = null;
        for (Expression expr: body) {
            last = expr.evaluate(letEnv);
        }
        return last;
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        Environment<TypeRef> letEnv = new Environment<>(env);
        for (Declaration decl: declarations) {
            TypeRef declTypeRef = new TypeRef();
            try {
                if (isLetStar) {
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

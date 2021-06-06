package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.runtime.SchemeRuntime;
import edu.vanderbilt.cs.wutkam.scheme.type.AbstractType;
import edu.vanderbilt.cs.wutkam.scheme.type.AbstractTypeDecl;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheme.type.UnifyException;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
            dec.define(letType, env, letEnv);
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
            decl.unify(letType, env, letEnv);
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

    public interface Declaration {

        void define(int letType, Environment<Expression> env, Environment<Expression> letEnv) throws LispException;
        void unify(int letType, Environment<TypeRef> env, Environment<TypeRef> letEnv) throws LispException;
    }

    public static class SymbolDeclaration implements Declaration {
        public String name;
        public Expression value;

        public SymbolDeclaration(String name, Expression value) {
            this.name = name;
            this.value = value;
        }

        @Override
        public void define(int letType, Environment<Expression> env, Environment<Expression> letEnv)
            throws LispException {
            if ((letType == LET_STAR_FORM) || (letType == LET_REC_FORM)) {
                if (letType == LET_REC_FORM) {
                    // Put the expression value into the environment before evaluating so that
                    // a let can contain a recursive function
                    letEnv.define(name, value);
                }
                // Both let* and letrec put the value into the environment so it is immediately visible to
                // other let definitions
                letEnv.define(name, value.evaluate(letEnv, false));
            } else {
                letEnv.define(name, value.evaluate(env, false));
            }
        }

        @Override
        public void unify(int letType, Environment<TypeRef> env, Environment<TypeRef> letEnv) throws LispException {
            TypeRef declTypeRef = new TypeRef();
            try {
                if ((letType == LET_STAR_FORM) || (letType == LET_REC_FORM)) {
                    // Put a typeref into the environment before unifying with the expression
                    // to allow the expression to refer to decl.name
                    letEnv.define(name, declTypeRef);
                    value.unify(declTypeRef, letEnv);
                } else {
                    value.unify(declTypeRef, env);
                }
            } catch (UnifyException exc) {
                throw UnifyException.addCause("Error unifying let expression "+name, exc);
            }
            letEnv.define(name, declTypeRef);
        }
    }

    public static class MatchDeclaration implements Declaration {
        public String typeName;
        public String constructorName;
        public String[] paramNames;
        public Expression value;

        public MatchDeclaration (String typeName, String constructorName,
            String[] paramNames, Expression value) {
            this.typeName = typeName;
            this.constructorName = constructorName;
            this.paramNames = paramNames;
            this.value = value;
        }

        @Override
        public void define(int letType, Environment<Expression> env, Environment<Expression> letEnv) throws LispException {
            Expression valueExpr;
            if ((letType == LET_STAR_FORM) || (letType == LET_REC_FORM)) {
                valueExpr = value.evaluate(letEnv, false);
            } else {
                valueExpr = value.evaluate(env, false);
            }
            AbstractTypeExpr abstractTypeExpr = (AbstractTypeExpr) valueExpr;

            for (int i=0; i < paramNames.length; i++) {
                if (paramNames[i].equals("_")) continue;

                if ((letType == LET_STAR_FORM) || (letType == LET_REC_FORM)) {
                    letEnv.define(paramNames[i], abstractTypeExpr.values.get(i));
                } else {
                    env.define(paramNames[i], abstractTypeExpr.values.get(i));
                }
            }
        }

        @Override
        public void unify(int letType, Environment<TypeRef> env, Environment<TypeRef> letEnv) throws LispException {
            AbstractTypeDecl abstractTypeDecl = SchemeRuntime.getTypeRegistry().lookup(typeName);
            TypeRef abstractTypeRef = new TypeRef(new AbstractType(abstractTypeDecl));
            if ((letType == LET_STAR_FORM) || (letType == LET_REC_FORM)) {
                value.unify(abstractTypeRef, letEnv);
            } else {
                value.unify(abstractTypeRef, env);
            }

            TypeConstructorExpr constructorFunc = abstractTypeDecl.typeConstructors.get(constructorName);
            if (constructorFunc == null) {
                throw new UnifyException("No constructor named "+constructorName+" in "+abstractTypeDecl);
            }

            Map<String,TypeRef> linkageMap = new HashMap<>();
            TypeRef[] parametricTypes = new TypeRef[constructorFunc.parametricTypes.size()];
            for (int i=0; i < parametricTypes.length; i++) {
                parametricTypes[i] = constructorFunc.parametricTypes.get(i).copy(linkageMap);
            }

            if (constructorFunc.paramTypes.length != paramNames.length) {
                throw new UnifyException("Constructor "+constructorName+" for type "+typeName+
                        " requires "+constructorFunc.paramTypes.length+" parameters")   ;
            }

            TypeRef[] paramTypes = new TypeRef[constructorFunc.paramTypes.length];
            for (int i=0; i < paramTypes.length; i++) {
                if (paramNames[i].equals("_")) continue;
                paramTypes[i] = constructorFunc.paramTypes[i].copy(linkageMap);
                if ((letType == LET_STAR_FORM) || (letType == LET_REC_FORM)) {
                    letEnv.define(paramNames[i], paramTypes[i]);
                } else {
                    env.define(paramNames[i], paramTypes[i]);
                }
            }
        }
    }
}

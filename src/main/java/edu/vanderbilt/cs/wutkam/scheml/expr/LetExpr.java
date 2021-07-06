package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;
import edu.vanderbilt.cs.wutkam.scheml.type.AbstractType;
import edu.vanderbilt.cs.wutkam.scheml.type.AbstractTypeDecl;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheml.type.UnifyException;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/** Represents a let expression, which can bind expressions to values and then execute
 * other expressions in the context of those bindings, similar to the way a function works.
 */
public class LetExpr implements Expression {

    // LET_FORM is the basic let where each expression being bound has no knowledge of the bindings that
    // came before it
    public static final int LET_FORM = 0;

    // LET_STAR_FORM allows subsequent bindings in the same let form to see the bindings that came before it
    public static final int LET_STAR_FORM = 1;

    // LET_REC_FORM allows a binding expression to see itself, so that a lambda expression being bound
    // can call itself recursively
    public static final int LET_REC_FORM = 2;

    // The declarations (bindings) in this let form
    List<Declaration> declarations;

    // Indicated whether this is a let, let*, or letrec
    int letType;

    // The body of the let expression
    List<Expression> body;

    public LetExpr(List<Declaration> declarations, int letType, List<Expression> body) {
        this.declarations = declarations;
        this.letType = letType;
        this.body = body;
    }

    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        Environment<Expression> letEnv = new Environment<>(env);

        // Bind all the declarations, each declaration handles evaluating the bound expression
        for (Declaration dec: declarations) {
            dec.define(letType, env, letEnv);
        }

        // Evaluate the body in the context of letEnv
        Expression last = null;
        for (int i=0; i < body.size(); i++) {
            Expression expr = body.get(i);
            if (inTailPosition && (i == body.size() - 1)) {
                last = expr.evaluate(letEnv, true);

            } else {
                last = expr.evaluate(letEnv, inTailPosition && (i == body.size() - 1));
            }
        }
        return last;
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {

        // Populate letEnv with type refs for each bound expression
        Environment<TypeRef> letEnv = new Environment<>(env);
        for (Declaration decl: declarations) {
            decl.unify(letType, env, letEnv);
        }

        // Unify each expression using the letEnv environment
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
        // The last expression is also the return type, so unify it against the requested type
        typeRef.unify(last);
    }

    /** Represents a type of let declaration */
    public interface Declaration {

        void define(int letType, Environment<Expression> env, Environment<Expression> letEnv) throws LispException;
        void unify(int letType, Environment<TypeRef> env, Environment<TypeRef> letEnv) throws LispException;
    }

    /** Represents a let declaration where an expression is bound to a single symbol */
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

    /** Represents a let declaration where an expression is bound to a value constructor */
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
                // no special processing for letrec here because letrec is really only useful for binding lambdas
                valueExpr = value.evaluate(letEnv, false);
            } else {
                valueExpr = value.evaluate(env, false);
            }
            AbstractTypeExpr abstractTypeExpr = (AbstractTypeExpr) valueExpr;

            // Once the value expression has been evaluated, it should be an abstract type expression
            // For each symbol give in the binding, bind that symbol to its corresponding value in the
            // abstract type expression and store it in the appropriate environment
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
            // Start with the abstract type declaration that this declaration uses
            AbstractTypeDecl abstractTypeDecl = SchemlRuntime.getTypeRegistry().lookup(typeName);
            TypeRef abstractTypeRef = new TypeRef(new AbstractType(abstractTypeDecl));

            // Unify that abstract type with the result from evaluating the value, which better
            // be the same kind of abstract type
            if ((letType == LET_STAR_FORM) || (letType == LET_REC_FORM)) {
                value.unify(abstractTypeRef, letEnv);
            } else {
                value.unify(abstractTypeRef, env);
            }

            ValueConstructorExpr constructorFunc = abstractTypeDecl.valueConstructors.get(constructorName);
            if (constructorFunc == null) {
                throw new UnifyException("No constructor named "+constructorName+" in "+abstractTypeDecl);
            }

            // Put the constructor functions parametric types into the linkage map
            // This will make sure that the references to 'a, 'b, etc. are linked up correctly
            Map<String,TypeRef> linkageMap = new HashMap<>();
            for (int i=0; i < constructorFunc.parametricTypes.size(); i++) {
                // we are throwing away the result, we just want the linkage map to know about the types
                // to properly remap them when they are used in parameters
                constructorFunc.parametricTypes.get(i).copy(linkageMap);
            }

            if (constructorFunc.paramTypes.length != paramNames.length) {
                throw new UnifyException("Constructor "+constructorName+" for type "+typeName+
                        " requires "+constructorFunc.paramTypes.length+" parameters")   ;
            }

            TypeRef[] paramTypes = new TypeRef[constructorFunc.paramTypes.length];
            for (int i=0; i < paramTypes.length; i++) {
                if (paramNames[i].equals("_")) continue;
                // Copy the param type and make sure it links up with the parametric types
                paramTypes[i] = constructorFunc.paramTypes[i].copy(linkageMap);

                // Store the param type ref in the appropriate environment variable
                if ((letType == LET_STAR_FORM) || (letType == LET_REC_FORM)) {
                    letEnv.define(paramNames[i], paramTypes[i]);
                } else {
                    env.define(paramNames[i], paramTypes[i]);
                }
            }
        }
    }
}

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

/** A match expression that evaluates an expression and matches it against a list of patterns
 */
public class MatchExpr implements Expression {
    AbstractTypeDecl abstractTypeDecl;
    Expression targetExpression;
    List<MatchTypeConstructor> matchPatterns;

    public MatchExpr(AbstractTypeDecl targetType, Expression targetExpression, List<MatchTypeConstructor> matchPatterns) {
        this.abstractTypeDecl = targetType;
        this.targetExpression = targetExpression;
        this.matchPatterns = matchPatterns;
    }

    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        // Evaluate the expression being matched
        Expression matchAgainst = targetExpression.evaluate(env, false);

        // Because of type checking, we know that this must be an AbstractTypeExpr
        AbstractTypeExpr abstractTypeExpr = (AbstractTypeExpr) matchAgainst;


        for (MatchTypeConstructor pattern: matchPatterns) {
            // Look for the pattern whose constructor name matches that of the expression being matched
            if (abstractTypeExpr.constructorName.equals(pattern.constructorName)) {
                // Evaluate the match expression
                return pattern.evaluate(abstractTypeExpr, env, inTailPosition);
            }
        }
        throw new LispException("Pattern match exhausted");
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        // Create a type ref for this abstract type
        TypeRef targetType = (new TypeRef(new AbstractType(abstractTypeDecl))).copy(new HashMap<>());
        try {
            // Unify the match expression against this abstract type
            targetExpression.unify(targetType, env);
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Unable to unify match expression", exc);
        }

        // Match each pattern against the requested type, since any pattern might be the one returned,
        // we need to make sure they all have the same return type, and all agree with the requested type
        for (MatchTypeConstructor pattern: matchPatterns) {
            pattern.unify(targetType, typeRef, env);
        }
    }

    /** Represents a type constructor within the context of a match expression
     */
    public static class MatchTypeConstructor {
        public String constructorName;
        public List<String> targetPatterns;
        public Expression targetExpression;

        public MatchTypeConstructor(String constructorName, List<String> targetPatterns, Expression targetExpression) {
            this.constructorName = constructorName;
            this.targetPatterns = targetPatterns;
            this.targetExpression = targetExpression;
        }

        public Expression evaluate(AbstractTypeExpr abstractTypeExpr, Environment<Expression> env, boolean inTailPosition)
            throws LispException {

            // Create an environment for binding the symbols in the type constructor pattern
            Environment<Expression> matchEnv = new Environment<>(env);
            for (int i=0; i < targetPatterns.size(); i++) {
                String itemName = targetPatterns.get(i);
                // A _ indicates we don't care about that value and we don't bother putting it in the environment
                if (itemName == "_") continue;
                // Attach the value from the specified field in the abstract type expression with the
                // symbol name in the pattern
                matchEnv.define(itemName, abstractTypeExpr.values.get(i));
            }
            // Evaluate the target expression for this pattern in the context of the matchEnv environment
            return targetExpression.evaluate(matchEnv, inTailPosition);
        }

        public void unify(TypeRef matchTargetType, TypeRef resultType, Environment<TypeRef> env) throws LispException {
            AbstractType matchType = (AbstractType) matchTargetType.getType();

            // Find the definition for this type
            AbstractTypeDecl abstractTypeDecl = SchemeRuntime.getTypeRegistry().lookup(matchType.typeName);

            // Find the type constructor for this pattern
            TypeConstructorExpr constructorFunc = abstractTypeDecl.typeConstructors.get(constructorName);
            if (constructorFunc == null) {
                throw new UnifyException("No constructor named "+constructorName+" in "+abstractTypeDecl);
            }

            // Make a copy of the parametric types and parameter types from the constructor fund
            Map<String,TypeRef> linkageMap = new HashMap<>();
            TypeRef[] parametricTypes = new TypeRef[constructorFunc.parametricTypes.size()];
            for (int i=0; i < parametricTypes.length; i++) {
                parametricTypes[i] = constructorFunc.parametricTypes.get(i).copy(linkageMap);
            }
            TypeRef[] paramTypes = new TypeRef[constructorFunc.paramTypes.length];
            for (int i=0; i < paramTypes.length; i++) {
                paramTypes[i] = constructorFunc.paramTypes[i].copy(linkageMap);
            }

            // Put each parameter type in the environment
            Environment<TypeRef> matchEnv = new Environment<>(env);
            for (int i=0; i < targetPatterns.size(); i++) {
                String itemName = targetPatterns.get(i);
                if (itemName == "_") continue;
                matchEnv.define(itemName, paramTypes[i]);
            }

            // Unify the target expression in the context of the matchEnv environment
            targetExpression.unify(resultType, matchEnv);

        }
    }
}

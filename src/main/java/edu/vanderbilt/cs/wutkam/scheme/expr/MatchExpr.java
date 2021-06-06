package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.match.MatchTypeConstructor;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.AbstractType;
import edu.vanderbilt.cs.wutkam.scheme.type.AbstractTypeDecl;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheme.type.UnifyException;

import java.util.HashMap;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 6/4/21
 * Time: 11:22 AM
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
        Expression matchAgainst = targetExpression.evaluate(env, false);
        AbstractTypeExpr abstractTypeExpr = (AbstractTypeExpr) matchAgainst;
        Environment<Expression> matchEnv = new Environment<>(env);
        for (MatchTypeConstructor pattern: matchPatterns) {
            if (abstractTypeExpr.constructorName.equals(pattern.constructorName)) {
                return pattern.evaluate(abstractTypeExpr, env, inTailPosition);
            }
        }
        throw new LispException("Pattern match exhausted");
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        TypeRef targetType = (new TypeRef(new AbstractType(abstractTypeDecl))).copy(new HashMap<>());
        try {
            targetExpression.unify(targetType, env);
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Unable to unify match expression", exc);
        }

        Environment<TypeRef> matchEnv = new Environment<>(env);
        for (MatchTypeConstructor pattern: matchPatterns) {
            pattern.unify(targetType, matchEnv);
        }
        targetExpression.unify(typeRef, matchEnv);
    }
}

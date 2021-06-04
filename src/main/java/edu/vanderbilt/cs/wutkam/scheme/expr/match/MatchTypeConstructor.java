package edu.vanderbilt.cs.wutkam.scheme.expr.match;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.ConsExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.AbstractType;
import edu.vanderbilt.cs.wutkam.scheme.type.FunctionType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheme.type.UnifyException;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 6/4/21
 * Time: 11:41 AM
 */
public class MatchTypeConstructor {
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

        Environment<Expression> matchEnv = new Environment<>(env);
        for (int i=0; i < targetPatterns.size(); i++) {
            String itemName = targetPatterns.get(i);
            if (itemName == "_") continue;
            matchEnv.define(itemName, abstractTypeExpr.values.get(i));
        }
        return targetExpression.evaluate(matchEnv, inTailPosition);
    }

    public void unify(TypeRef matchTargetType, Environment<TypeRef> env) throws LispException {
        AbstractType abstractType = (AbstractType) matchTargetType.getType();
        FunctionType constructorFunc = abstractType.typeConstructors.get(constructorName);
        if (constructorFunc == null) {
            throw new UnifyException("No constructor named "+constructorName+" in "+abstractType);
        }

        for (int i=0; i < targetPatterns.size(); i++) {
            String itemName = targetPatterns.get(i);
            if (itemName == "_") continue;
            TypeRef typeRef = constructorFunc.paramTypes[i];
            env.define(itemName, typeRef);
        }
    }
}

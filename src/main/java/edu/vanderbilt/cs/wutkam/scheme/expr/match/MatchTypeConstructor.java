package edu.vanderbilt.cs.wutkam.scheme.expr.match;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.ConsExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.TypeConstructorExpr;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.runtime.SchemeRuntime;
import edu.vanderbilt.cs.wutkam.scheme.type.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

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

    public void unify(TypeRef matchTargetType, TypeRef resultType, Environment<TypeRef> env) throws LispException {
        AbstractType matchType = (AbstractType) matchTargetType.getType();
        AbstractTypeDecl abstractTypeDecl = SchemeRuntime.getTypeRegistry().lookup(matchType.typeName);
        TypeConstructorExpr constructorFunc = abstractTypeDecl.typeConstructors.get(constructorName);
        if (constructorFunc == null) {
            throw new UnifyException("No constructor named "+constructorName+" in "+abstractTypeDecl);
        }

        Map<String,TypeRef> linkageMap = new HashMap<>();
        TypeRef[] parametricTypes = new TypeRef[constructorFunc.parametricTypes.size()];
        for (int i=0; i < parametricTypes.length; i++) {
            parametricTypes[i] = constructorFunc.parametricTypes.get(i).copy(linkageMap);
        }
        TypeRef[] paramTypes = new TypeRef[constructorFunc.paramTypes.length];
        for (int i=0; i < paramTypes.length; i++) {
            paramTypes[i] = constructorFunc.paramTypes[i].copy(linkageMap);
        }

        Environment<TypeRef> matchEnv = new Environment<>(env);
        for (int i=0; i < targetPatterns.size(); i++) {
            String itemName = targetPatterns.get(i);
            if (itemName == "_") continue;
            TypeRef typeRef = paramTypes[i];
            matchEnv.define(itemName, typeRef);
        }

        targetExpression.unify(resultType, matchEnv);
    }
}

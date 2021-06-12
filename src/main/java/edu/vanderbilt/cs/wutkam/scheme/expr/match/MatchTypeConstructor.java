package edu.vanderbilt.cs.wutkam.scheme.expr.match;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.TypeConstructorExpr;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.runtime.SchemlRuntime;
import edu.vanderbilt.cs.wutkam.scheme.type.AbstractType;
import edu.vanderbilt.cs.wutkam.scheme.type.AbstractTypeDecl;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheme.type.UnifyException;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Represents a type constructor within the context of a match expression
 */
public class MatchTypeConstructor implements Match {
    public String constructorName;
    public List<Match> targetPatterns;

    public MatchTypeConstructor(String constructorName, List<Match> targetPatterns) {
        this.constructorName = constructorName;
        this.targetPatterns = targetPatterns;
    }

    @Override
    public boolean matches(Expression expr) {
        AbstractTypeExpr abstractTypeExpr = (AbstractTypeExpr) expr;
        if (!abstractTypeExpr.constructorName.equals(constructorName)) return false;

        for (int i=0; i < targetPatterns.size(); i++) {
            if (!targetPatterns.get(i).matches(abstractTypeExpr.values.get(i))) return false;
        }
        return true;
    }

    @Override
    public void defineEnvironment(Expression expr, Environment<Expression> env) {
        AbstractTypeExpr abstractTypeExpr = (AbstractTypeExpr) expr;

        for (int i = 0; i < targetPatterns.size(); i++) {
            Match item = targetPatterns.get(i);
            item.defineEnvironment(abstractTypeExpr.values.get(i), env);
        }
    }

    @Override
    public void unify(TypeRef matchTargetType, Environment<TypeRef> env) throws LispException {
        // Find the definition for this type
        AbstractTypeDecl abstractTypeDecl = SchemlRuntime.getTypeRegistry().findByConstructor(constructorName);

        // Find the type constructor for this pattern
        TypeConstructorExpr constructorFunc = abstractTypeDecl.typeConstructors.get(constructorName);
        if (constructorFunc == null) {
            throw new UnifyException("No constructor named " + constructorName + " in " + abstractTypeDecl);
        }

        matchTargetType.unify(new TypeRef(new AbstractType(abstractTypeDecl)));

        // Make a copy of the parametric types and parameter types from the constructor fund
        Map<String, TypeRef> linkageMap = new HashMap<>();
        TypeRef[] parametricTypes = new TypeRef[constructorFunc.parametricTypes.size()];
        for (int i = 0; i < parametricTypes.length; i++) {
            parametricTypes[i] = constructorFunc.parametricTypes.get(i).copy(linkageMap);
        }
        TypeRef[] paramTypes = new TypeRef[constructorFunc.paramTypes.length];
        for (int i = 0; i < paramTypes.length; i++) {
            paramTypes[i] = constructorFunc.paramTypes[i].copy(linkageMap);
        }

        for (int i=0; i < paramTypes.length; i++) {
            targetPatterns.get(i).unify(paramTypes[i], env);
        }
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        if (constructorName.equals("Nil")) {
            return "nil";
        } else if (constructorName.equals("Cons")) {
            builder.append('(');
            boolean isFirst = true;
            MatchTypeConstructor curr = this;
            while (curr.constructorName.equals("Cons")) {
                if (!isFirst) builder.append(" ");
                isFirst = false;
                builder.append(curr.targetPatterns.get(0).toString());
                Match tail = curr.targetPatterns.get(1);
                if (tail instanceof MatchTypeConstructor) {
                    curr = (MatchTypeConstructor) curr.targetPatterns.get(1);
                } else {
                    break;
                }
            }
            builder.append(')');
            return builder.toString();
        } else {
            builder.append('(');
            builder.append(constructorName);
            for (Match match : targetPatterns) {
                builder.append(' ');
                builder.append(match.toString());
            }
            builder.append(')');
            return builder.toString();
        }
    }
}

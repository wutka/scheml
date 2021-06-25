package edu.vanderbilt.cs.wutkam.scheml.expr.match;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.TypeConstructorExpr;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;
import edu.vanderbilt.cs.wutkam.scheml.type.AbstractType;
import edu.vanderbilt.cs.wutkam.scheml.type.AbstractTypeDecl;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheml.type.UnifyException;

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

        // If this abstract type was constructed with a different constructor, it doesn't match
        if (!abstractTypeExpr.constructorName.equals(constructorName)) return false;

        // Try to match the target patterns in the expression with the target patterns in this matcher
        for (int i=0; i < targetPatterns.size(); i++) {
            if (!targetPatterns.get(i).matches(abstractTypeExpr.values.get(i))) return false;
        }
        return true;
    }

    @Override
    public void defineEnvironment(Expression expr, Environment<Expression> env) {
        AbstractTypeExpr abstractTypeExpr = (AbstractTypeExpr) expr;

        // For each target pattern, ask them to define themselves in the environment
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


        // Make a copy of the parametric types and parameter types from the constructor fund
        Map<String, TypeRef> linkageMap = new HashMap<>();

        AbstractType abstractType = (AbstractType) (new AbstractType(abstractTypeDecl)).copy(linkageMap);
        // Make sure the target type matches the type of the abstract type
        matchTargetType.unify(new TypeRef(abstractType));


        // Although we don't do anything with the parametric types, we do copy them to make
        // sure the linkage map is consistent
        TypeRef[] parametricTypes = new TypeRef[constructorFunc.parametricTypes.size()];
        for (int i = 0; i < parametricTypes.length; i++) {
            parametricTypes[i] = constructorFunc.parametricTypes.get(i).copy(linkageMap);
            parametricTypes[i].unify(abstractType.typeParameters.get(i));
        }

        // Copy each of the param types
        TypeRef[] paramTypes = new TypeRef[constructorFunc.paramTypes.length];
        for (int i = 0; i < paramTypes.length; i++) {
            paramTypes[i] = constructorFunc.paramTypes[i].copy(linkageMap);
        }

        // Unify against all of the target patterns
        for (int i=0; i < paramTypes.length; i++) {
            targetPatterns.get(i).unify(paramTypes[i], env);
        }
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        // Although the ConsTypeDecl has a custom toString method for an expression, we just
        // do the toString for the Cons type matcher here
        if (constructorName.equals("Nil")) {
            return "nil";
        } else if (constructorName.equals("Cons")) {
            builder.append('(');
            boolean isFirst = true;
            MatchTypeConstructor curr = this;

            // loop through the list
            while (curr.constructorName.equals("Cons")) {
                if (!isFirst) builder.append(" ");
                isFirst = false;

                // Append the string value of the current cell to the builder
                builder.append(curr.targetPatterns.get(0).toString());

                // get the next cell in the list
                Match tail = curr.targetPatterns.get(1);
                if (tail instanceof MatchTypeConstructor) {
                    // the next cell should be a match constructor
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

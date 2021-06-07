package edu.vanderbilt.cs.wutkam.scheme.forms;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.*;
import edu.vanderbilt.cs.wutkam.scheme.runtime.SchemeRuntime;
import edu.vanderbilt.cs.wutkam.scheme.type.AbstractTypeDecl;
import edu.vanderbilt.cs.wutkam.scheme.type.UnifyException;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/** Provides a pattern-matching form for abstract types.
 * Only very basic patterns are provided in that you can only provide symbols in the type constructor
 * part of the match, it won't match against values. This is because match doesn't currently have a robust
 * exhaustiveness checker and only checks to make sure each type constructor is present. Expanding the
 * exhaustiveness checker is left as a future exercise
 *
 */
public class MatchForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        if (aList.size() < 3) {
            throw new LispException("match expression must have an expression and at least one match pattern");
        }
        Expression targetExpr = aList.getElement(1);
        if (targetExpr instanceof ListExpr) {
            targetExpr = FormExpander.expand((ListExpr) targetExpr, false);
        }

        AbstractTypeDecl existingType = null;

        List<MatchExpr.MatchTypeConstructor> typeConstructors = new ArrayList<>();
        Set<String> definedPatterns = new HashSet<>();

        for (Expression patternExpr: aList.elementsFrom(2)) {

            if (!(patternExpr instanceof ListExpr)) {
                throw new LispException("pattern in match expression must be a list");
            }

            ListExpr pattern = (ListExpr) patternExpr;
            if (pattern.size() != 2) {
                throw new LispException("match pattern must contain a pattern and an expression");
            }

            Expression matchExpr = pattern.getElement(0);

            // For a type constructor that has no arguments, you can just provide the name here without
            // putting it in a list. In other words, for (type cons-type ('a) Null (Cons 'a (cons-type 'a)))
            // you can do:
            // (match (get-some-cons x)
            //   (Null (printf "it is null\n"))
            //   ((Cons _ _) (printf "it has data\n"))

            String matchName;
            AbstractTypeDecl type;
            List<String> targetPatterns = new ArrayList<>();

            // Get the name of the type constructor so we can find out the type being matched
            if (matchExpr instanceof SymbolExpr) {
                matchName = ((SymbolExpr) matchExpr).value;
            } else if (matchExpr instanceof ListExpr) {

                ListExpr matchList = (ListExpr) matchExpr;
                if (matchList.size() == 0) {
                    throw new LispException("Match pattern must contain at least one element");
                }

                if (!(matchList.getElement(0) instanceof SymbolExpr)) {
                    throw new LispException("constructor name in match pattern must be a symbol");
                }

                matchName = ((SymbolExpr) matchList.getElement(0)).value;
            } else {
                throw new LispException("Match pattern must be a symbol or a list");
            }

            if (definedPatterns.contains(matchName)) {
                throw new LispException("pattern list already contains "+matchName);
            }

            // Find the type that has this constructor
            type = SchemeRuntime.getTypeRegistry().findByConstructor(matchName);
            if (type == null) {
                throw new LispException("type constructor name "+matchName+" doesn't belong to any existing type");
            } else {
                // Make sure it is consistent with the other constructors in the match
                if ((existingType != null) && (!existingType.typeName.equals(type.typeName))) {
                    throw new UnifyException("Previous match patterns match type " + existingType.typeName + " but " +
                            matchName + " belongs to type " + type.typeName);
                }
                existingType = type;
            }

            if (matchExpr instanceof SymbolExpr) {
                // For a symbol-only match, the constructor can't take any parameters
                TypeConstructorExpr constructorFunc = existingType.typeConstructors.get(matchName);
                if (constructorFunc.paramTypes.length > 0) {
                    throw new LispException("Match pattern contains only a symbol, but the corresponding type "+
                            "constructor takes "+constructorFunc.paramTypes.length+" parameters");
                }

            } else if (matchExpr instanceof ListExpr) {
                ListExpr matchList = (ListExpr) matchExpr;

                // Make sure the argument counts match
                TypeConstructorExpr constructorFunc = existingType.typeConstructors.get(matchName);
                if (constructorFunc.paramTypes.length != matchList.size()-1) {
                    throw new LispException("match pattern with constructor "+matchName+" contains "+
                            (matchList.size()-1)+" parameters, but constructor for type requires exactly "+
                            constructorFunc.paramTypes.length);
                }

                // Make sure all the elements in the constructor are symbols
                List<Expression> constructorSyms = matchList.elementsFrom(1);

                for (Expression expr: constructorSyms) {
                    if (!(expr instanceof SymbolExpr)) {
                        throw new LispException("all arguments to a pattern match constructor must be symbols");
                    }
                    targetPatterns.add(((SymbolExpr)expr).value);
                }
            }

            // Expand the target expression if necessary
            Expression patternTargetExpr = pattern.getElement(1);
            if (patternTargetExpr instanceof ListExpr) {
                patternTargetExpr = FormExpander.expand((ListExpr) patternTargetExpr, false);
            }

            typeConstructors.add(new MatchExpr.MatchTypeConstructor(matchName, targetPatterns, patternTargetExpr));
            definedPatterns.add(matchName);
        }
        
        if (existingType.typeConstructors.size() != definedPatterns.size()) {
            for (String constructorName: existingType.typeConstructors.keySet()) {
                if (!definedPatterns.contains(constructorName)) {
                    throw new LispException("match form is missing constructor "+constructorName);
                }
            }
        }

        return new MatchExpr(existingType, targetExpr, typeConstructors);
    }
}

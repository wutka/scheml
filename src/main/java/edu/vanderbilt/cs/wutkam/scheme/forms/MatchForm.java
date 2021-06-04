package edu.vanderbilt.cs.wutkam.scheme.forms;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.FunctionExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.MatchExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.SymbolExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.match.MatchTypeConstructor;
import edu.vanderbilt.cs.wutkam.scheme.runtime.SchemeRuntime;
import edu.vanderbilt.cs.wutkam.scheme.type.AbstractType;
import edu.vanderbilt.cs.wutkam.scheme.type.FunctionType;
import edu.vanderbilt.cs.wutkam.scheme.type.UnifyException;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 6/4/21
 * Time: 4:40 PM
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

        AbstractType existingType = null;

        List<MatchTypeConstructor> typeConstructors = new ArrayList<>();
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
            if (matchExpr instanceof SymbolExpr) {
                String matchName = ((SymbolExpr)matchExpr).value;
                if (definedPatterns.contains(matchName)) {
                    throw new LispException("pattern list already contains "+matchName);
                }
                AbstractType type = SchemeRuntime.getTypeRegistry().findByConstructor(matchName);
                if (type == null) {
                    throw new LispException("type constructor name "+matchName+" doesn't belong to any existing type");
                } else {
                    if ((existingType != null) && (!existingType.typeName.equals(type.typeName))) {
                        throw new UnifyException("Previous match patterns match type " + existingType.typeName + " but " +
                                matchName + " belongs to type " + type.typeName);
                    }
                    existingType = type;
                }
                FunctionType constructorFunc = existingType.typeConstructors.get(matchName);
                if (constructorFunc.paramTypes.length > 0) {
                    throw new LispException("Match pattern contains only a symbol, but the corresponding type "+
                            "constructor takes "+constructorFunc.paramTypes.length+" parameters");
                }
                Expression patternTargetExpr = pattern.getElement(1);
                if (patternTargetExpr instanceof ListExpr) {
                    patternTargetExpr = FormExpander.expand((ListExpr) patternTargetExpr, false);
                }
                typeConstructors.add(new MatchTypeConstructor(matchName, new ArrayList<>(), patternTargetExpr));
                definedPatterns.add(matchName);
            } else if (matchExpr instanceof ListExpr) {
                ListExpr matchList = (ListExpr) matchExpr;
                if (matchList.size() == 0) {
                    throw new LispException("Match pattern must contain at least one element");
                }

                Expression matchNameExpr = matchList.getElement(0);
                if (!(matchNameExpr instanceof SymbolExpr)) {
                    throw new LispException("constructor name in match pattern must be a symbol");
                }

                String matchName = ((SymbolExpr)matchNameExpr).value;

                if (definedPatterns.contains(matchName)) {
                    throw new LispException("pattern list already contains "+matchName);
                }
                AbstractType type = SchemeRuntime.getTypeRegistry().findByConstructor(matchName);
                if (type == null) {
                    throw new LispException("type constructor name "+matchName+" doesn't belong to any existing type");
                } else { if ((existingType != null) && (!existingType.typeName.equals(type.typeName))) { throw new UnifyException("Previous match patterns match type " + existingType.typeName + " but " + matchName + " belongs to type " + type.typeName); } existingType = type;
                }
                FunctionType constructorFunc = existingType.typeConstructors.get(matchName);
                if (constructorFunc.paramTypes.length != matchList.size()-1) {
                    throw new LispException("match pattern with constructor "+matchName+" contains "+
                            (matchList.size()-1)+" parameters, but constructor for type requires exactly "+
                            constructorFunc.paramTypes.length);
                }
                List<Expression> constructorSyms = matchList.elementsFrom(1);
                List<String> targetPatterns = new ArrayList<>();
                for (Expression expr: constructorSyms) {
                    if (!(expr instanceof SymbolExpr)) {
                        throw new LispException("all arguments to a pattern match constructor must be symbols");
                    }
                    targetPatterns.add(((SymbolExpr)expr).value);
                }

                Expression patternTargetExpr = pattern.getElement(1);
                if (patternTargetExpr instanceof ListExpr) {
                    patternTargetExpr = FormExpander.expand((ListExpr) patternTargetExpr, false);
                }
                typeConstructors.add(new MatchTypeConstructor(matchName, targetPatterns, patternTargetExpr));
                definedPatterns.add(matchName);
            } else {
                throw new LispException("pattern must contain either a single symbol or a list");
            }
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

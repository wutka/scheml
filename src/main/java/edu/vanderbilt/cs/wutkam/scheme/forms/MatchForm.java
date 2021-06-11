package edu.vanderbilt.cs.wutkam.scheme.forms;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.*;
import edu.vanderbilt.cs.wutkam.scheme.expr.match.ExhaustivenessChecker;
import edu.vanderbilt.cs.wutkam.scheme.expr.match.Match;
import edu.vanderbilt.cs.wutkam.scheme.expr.match.MatchBool;
import edu.vanderbilt.cs.wutkam.scheme.expr.match.MatchChar;
import edu.vanderbilt.cs.wutkam.scheme.expr.match.MatchDouble;
import edu.vanderbilt.cs.wutkam.scheme.expr.match.MatchInt;
import edu.vanderbilt.cs.wutkam.scheme.expr.match.MatchString;
import edu.vanderbilt.cs.wutkam.scheme.expr.match.MatchTypeConstructor;
import edu.vanderbilt.cs.wutkam.scheme.expr.match.MatchVariable;
import edu.vanderbilt.cs.wutkam.scheme.runtime.SchemeRuntime;
import edu.vanderbilt.cs.wutkam.scheme.type.AbstractType;
import edu.vanderbilt.cs.wutkam.scheme.type.AbstractTypeDecl;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheme.type.UnifyException;

import java.util.ArrayList;
import java.util.Arrays;
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

        List<MatchExpr.MatchPatternAndTarget> patterns = new ArrayList<>();
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

            Match matchPattern = parseMatchPattern(matchExpr);
            Expression matchTargetExpression = pattern.getElement(1);
            if (matchTargetExpression instanceof ListExpr) {
                matchTargetExpression = FormExpander.expand((ListExpr) matchTargetExpression, false);
            }
            patterns.add(new MatchExpr.MatchPatternAndTarget(matchPattern, matchTargetExpression));
        }

        return new MatchExpr(targetExpr, patterns);
    }

    protected Match parseMatchPattern(Expression expr) throws LispException {
        if (expr instanceof ListExpr) {   // Type constructor
            ListExpr listExpr = (ListExpr) expr;
            if (listExpr.size() >= 1 && (listExpr.getElement(0) instanceof SymbolExpr)) {
                String constructorName = ((SymbolExpr)listExpr.getElement(0)).value;
                AbstractTypeDecl abstractTypeDecl = SchemeRuntime.getTypeRegistry().findByConstructor(constructorName);

                if (abstractTypeDecl != null) {
                    TypeConstructorExpr typeConstructorExpr = abstractTypeDecl.typeConstructors.get(constructorName);

                    if (listExpr.size() - 1 != typeConstructorExpr.paramTypes.length) {
                        throw new LispException("Type constructor " + constructorName + " takes exactly " +
                                typeConstructorExpr.paramTypes.length + " parameters");
                    }

                    List<Match> patterns = new ArrayList<>();
                    for (Expression patternExpr : listExpr.elementsFrom(1)) {
                        patterns.add(parseMatchPattern(patternExpr));
                    }

                    return new MatchTypeConstructor(constructorName, patterns);
                }
            }

            // If it isn't a type constructor, treat it as a list and generate the necessary cons matchers
            Match curr = parseMatchPattern(new SymbolExpr("Nil"));
            for (int i=listExpr.size()-1; i >= 0; i--) {
                Match itemMatch = parseMatchPattern(listExpr.getElement(i));
                curr = new MatchTypeConstructor("Cons", Arrays.asList(itemMatch, curr));
            }
            return curr;

        } else if (expr instanceof BoolExpr) {
            return new MatchBool(((BoolExpr) expr).value);
        } else if (expr instanceof CharExpr) {
            return new MatchChar(((CharExpr) expr).value);
        } else if (expr instanceof DoubleExpr) {
            return new MatchDouble(((DoubleExpr) expr).value);
        } else if (expr instanceof IntExpr) {
            return new MatchInt(((IntExpr) expr).value);
        } else if (expr instanceof StringExpr) {
            return new MatchString(((StringExpr) expr).value);
        } else if (expr instanceof SymbolExpr) {
            String name = ((SymbolExpr)expr).value;
            AbstractTypeDecl abstractTypeDecl = SchemeRuntime.getTypeRegistry().findByConstructor(name);

            if (abstractTypeDecl != null) {
                TypeConstructorExpr typeConstructorExpr = abstractTypeDecl.typeConstructors.get(name);

                List<Match> patterns = new ArrayList<>();

                return new MatchTypeConstructor(name, patterns);
            } else {
                return new MatchVariable(name);
            }
        } else {
            throw new LispException("Invalid expression in match pattern: "+expr.toString());
        }
    }
}

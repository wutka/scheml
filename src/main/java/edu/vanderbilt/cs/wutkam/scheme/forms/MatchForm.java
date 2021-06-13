package edu.vanderbilt.cs.wutkam.scheme.forms;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.*;
import edu.vanderbilt.cs.wutkam.scheme.expr.match.Match;
import edu.vanderbilt.cs.wutkam.scheme.expr.match.MatchBool;
import edu.vanderbilt.cs.wutkam.scheme.expr.match.MatchChar;
import edu.vanderbilt.cs.wutkam.scheme.expr.match.MatchDouble;
import edu.vanderbilt.cs.wutkam.scheme.expr.match.MatchInt;
import edu.vanderbilt.cs.wutkam.scheme.expr.match.MatchString;
import edu.vanderbilt.cs.wutkam.scheme.expr.match.MatchTypeConstructor;
import edu.vanderbilt.cs.wutkam.scheme.expr.match.MatchVariable;
import edu.vanderbilt.cs.wutkam.scheme.runtime.SchemlRuntime;
import edu.vanderbilt.cs.wutkam.scheme.type.AbstractTypeDecl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/** Provides nested pattern matching. You can match against simple values or type constructors where each
 * target value of a type constructor can be a match expression, so they can nest as far down as necessary.
 * There is a simple syntax for matching lists where if a match starts with a ( and the next symbol is
 * not a type constructor named, it is assumed to be a list, and will be translated automatically into the
 * correct combination of MatchTypeConstructor instances
 */
public class MatchForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        if (aList.size() < 3) {
            throw new LispException("match expression must have an expression and at least one match pattern");
        }
        Expression targetExpr = aList.getElement(1);

        // If the target expression is a list, expand it
        if (targetExpr instanceof ListExpr) {
            targetExpr = FormExpander.expand((ListExpr) targetExpr, false);
        }

        List<MatchExpr.MatchPatternAndTarget> patterns = new ArrayList<>();

        // parse each pattern & expression
        for (Expression patternExpr: aList.elementsFrom(2)) {

            // The pattern must be a list
            if (!(patternExpr instanceof ListExpr)) {
                throw new LispException("pattern in match expression must be a list containing the "+
                        "match pattern and resulting expression");
            }

            ListExpr pattern = (ListExpr) patternExpr;
            if (pattern.size() != 2) {
                throw new LispException("match pattern must contain a pattern and an expression");
            }

            Expression matchExpr = pattern.getElement(0);

            // For a type constructor that has no arguments, you can just provide the name here without
            // putting it in a list. In other words, for (type cons-type ('a) Nil (Cons 'a (cons-type 'a)))
            // you can do:
            // (match (get-some-cons x)
            //   (Nil (printf "it is nil\n"))
            //   ((Cons _ _) (printf "it has data\n"))

            // parse the match expression, recursively if necessary
            Match matchPattern = parseMatchPattern(matchExpr);

            Expression matchTargetExpression = pattern.getElement(1);
            if (matchTargetExpression instanceof ListExpr) {
                // Expand the target expression for each match
                matchTargetExpression = FormExpander.expand((ListExpr) matchTargetExpression, false);
            }
            patterns.add(new MatchExpr.MatchPatternAndTarget(matchPattern, matchTargetExpression));
        }

        return new MatchExpr(targetExpr, patterns);
    }

    /** Parses a match pattern, which may include matches that need to be parsed recursively */
    protected Match parseMatchPattern(Expression expr) throws LispException {
        if (expr instanceof ListExpr) {   // Type constructor or list
            ListExpr listExpr = (ListExpr) expr;

            // See if the list starts with a symbol
            if (listExpr.size() >= 1 && (listExpr.getElement(0) instanceof SymbolExpr)) {
                String constructorName = ((SymbolExpr)listExpr.getElement(0)).value;
                AbstractTypeDecl abstractTypeDecl = SchemlRuntime.getTypeRegistry().findByConstructor(constructorName);

                // If the symbol is the name of a type constructor, treat this list like a type constructor
                if (abstractTypeDecl != null) {
                    TypeConstructorExpr typeConstructorExpr = abstractTypeDecl.typeConstructors.get(constructorName);

                    if (listExpr.size() - 1 != typeConstructorExpr.paramTypes.length) {
                        throw new LispException("Type constructor " + constructorName + " takes exactly " +
                                typeConstructorExpr.paramTypes.length + " parameters");
                    }

                    // Recursively parse the match pattern for each target expression
                    List<Match> patterns = new ArrayList<>();
                    for (Expression patternExpr : listExpr.elementsFrom(1)) {
                        patterns.add(parseMatchPattern(patternExpr));
                    }

                    return new MatchTypeConstructor(constructorName, patterns);
                }
            }

            // If it isn't a type constructor, treat it as a list and generate the necessary cons matchers
            Match curr = parseMatchPattern(new SymbolExpr("Nil"));

            // Start from the end of the list because it is easier to build the list from the end
            for (int i=listExpr.size()-1; i >= 0; i--) {
                // recursively parse each match pattern
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
            // We allow matching against a no-arg type constructor without extra parens
            // so (Nil (print "it is nil")) is easier than ((Nil) (print "it is nil"))
            String name = ((SymbolExpr) expr).value;
            AbstractTypeDecl abstractTypeDecl = SchemlRuntime.getTypeRegistry().findByConstructor(name);

            if (abstractTypeDecl != null) {
                TypeConstructorExpr typeConstructorExpr = abstractTypeDecl.typeConstructors.get(name);

                if (typeConstructorExpr.paramTypes.length > 0) {
                    throw new LispException("Must use a list for type constructor "+name+
                            " because it requires "+typeConstructorExpr.paramTypes.length+" arguments");
                }
                List<Match> patterns = new ArrayList<>();

                return new MatchTypeConstructor(name, patterns);
            } else {
                return new MatchVariable(name);
            }
        } else if (expr instanceof AbstractTypeExpr) {
            AbstractTypeExpr abstractTypeExpr = (AbstractTypeExpr) expr;
            if (abstractTypeExpr.constructorName.equals("Nil")) {
                return parseMatchPattern(new SymbolExpr("Nil"));
            }
            throw new LispException("Invalid expression in match pattern: "+ expr);
        } else {
            throw new LispException("Invalid expression in match pattern: "+expr.toString());
        }
    }
}

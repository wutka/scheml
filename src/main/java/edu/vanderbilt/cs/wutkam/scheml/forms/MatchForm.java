package edu.vanderbilt.cs.wutkam.scheml.forms;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.*;
import edu.vanderbilt.cs.wutkam.scheml.expr.match.Match;
import edu.vanderbilt.cs.wutkam.scheml.expr.match.MatchBool;
import edu.vanderbilt.cs.wutkam.scheml.expr.match.MatchChar;
import edu.vanderbilt.cs.wutkam.scheml.expr.match.MatchDouble;
import edu.vanderbilt.cs.wutkam.scheml.expr.match.MatchInt;
import edu.vanderbilt.cs.wutkam.scheml.expr.match.MatchString;
import edu.vanderbilt.cs.wutkam.scheml.expr.match.MatchSymbol;
import edu.vanderbilt.cs.wutkam.scheml.expr.match.MatchValueConstructor;
import edu.vanderbilt.cs.wutkam.scheml.expr.match.MatchVariable;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;
import edu.vanderbilt.cs.wutkam.scheml.type.AbstractTypeDecl;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Stack;

/** Provides nested pattern matching. You can match against simple values or value constructors where each
 * target value of a value constructor can be a match expression, so they can nest as far down as necessary.
 * There is a simple syntax for matching lists where if a match starts with a ( and the next symbol is
 * not a value constructor named, it is assumed to be a list, and will be translated automatically into the
 * correct combination of MatchValueConstructor instances
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

            // For a value constructor that has no arguments, you can just provide the name here without
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
        if (expr instanceof ListExpr) {   // Value constructor or list
            ListExpr listExpr = (ListExpr) expr;

            // See if the list starts with a symbol
            if (listExpr.size() >= 1 && (listExpr.getElement(0) instanceof SymbolExpr)) {
                String symbol = ((SymbolExpr)listExpr.getElement(0)).value;

                if (symbol.equals("quote")) {
                    if (listExpr.getElement(1) instanceof ListExpr) {
                        return quotedToMatchPatterns((ListExpr) listExpr.getElement(1));
                    } else if (listExpr.getElement(1) instanceof SymbolExpr) {
                        String varStr = ((SymbolExpr) listExpr.getElement(1)).value;
                        return new MatchValueConstructor("SexprSymbol", Arrays.asList(new MatchVariable(varStr)));
                    }
                } else if (isSexprTypeName(symbol)) {
                    return parseSexprMatchVariable(listExpr);
                } else {
                    AbstractTypeDecl abstractTypeDecl = SchemlRuntime.getTypeRegistry().
                            findByConstructor(symbol);

                    // If the symbol is the name of a value constructor, treat this list like a value constructor
                    if (abstractTypeDecl != null) {
                        ValueConstructorExpr valueConstructorExpr = abstractTypeDecl.valueConstructors
                                .get(symbol);

                        if (listExpr.size() - 1 != valueConstructorExpr.paramTypes.length) {
                            throw new LispException("Value constructor " + symbol + " takes exactly " +
                                    valueConstructorExpr.paramTypes.length + " parameters");
                        }

                        // Recursively parse the match pattern for each target expression
                        List<Match> patterns = new ArrayList<>();
                        for (Expression patternExpr : listExpr.elementsFrom(1)) {
                            patterns.add(parseMatchPattern(patternExpr));
                        }

                        return new MatchValueConstructor(symbol, patterns);
                    }
                }
            }

            // If it isn't a value constructor, treat it as a list and generate the necessary cons matchers
            Match curr;
            int start;

            if (listExpr.size() < 2 || !(listExpr.getElement(listExpr.size()-2) instanceof SymbolExpr) ||
                !((SymbolExpr)listExpr.getElement(listExpr.size()-2)).value.equals("&rest")) {
                curr = parseMatchPattern(new SymbolExpr("Nil"));
                start = listExpr.size()-1;
            } else {
                curr = parseMatchPattern(listExpr.getElement(listExpr.size()-1));
                start = listExpr.size()-3;
            }

            // Start from the end of the list because it is easier to build the list from the end
            for (int i=start; i >= 0; i--) {
                // recursively parse each match pattern
                Match itemMatch = parseMatchPattern(listExpr.getElement(i));
                curr = new MatchValueConstructor("Cons", Arrays.asList(itemMatch, curr));
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
            // We allow matching against a no-arg value constructor without extra parens
            // so (Nil (print "it is nil")) is easier than ((Nil) (print "it is nil"))
            String name = ((SymbolExpr) expr).value;
            AbstractTypeDecl abstractTypeDecl = SchemlRuntime.getTypeRegistry().findByConstructor(name);

            if (abstractTypeDecl != null) {
                ValueConstructorExpr valueConstructorExpr = abstractTypeDecl.valueConstructors.get(name);

                if (valueConstructorExpr.paramTypes.length > 0) {
                    throw new LispException("Must use a list for value constructor "+name+
                            " because it requires "+ valueConstructorExpr.paramTypes.length+" arguments");
                }
                List<Match> patterns = new ArrayList<>();

                return new MatchValueConstructor(name, patterns);
            } else {
                return new MatchVariable(name);
            }
        } else if (expr instanceof AbstractTypeExpr) {
            AbstractTypeExpr abstractTypeExpr = (AbstractTypeExpr) expr;
            if (abstractTypeExpr.constructorName.equals("Nil")) {
                return parseMatchPattern(new SymbolExpr("Nil"));
            } else if (abstractTypeExpr.constructorName.equals("SexprSymbol")) {
                SymbolLiteralExpr litSym = (SymbolLiteralExpr)abstractTypeExpr.values.get(0);
                SymbolExpr sym = new SymbolExpr(litSym.value);
                return new MatchValueConstructor("SexprSymbol", Arrays.asList(new MatchSymbol(sym)));
            }
            throw new LispException("Invalid expression in match pattern: "+ expr);
        } else {
            throw new LispException("Invalid expression in match pattern: "+expr.toString());
        }
    }

    protected Match quotedToMatchPatterns(ListExpr list) throws LispException {
        List<Match> matches = new ArrayList<>();
        boolean lastIsSplice = false;

        for (int i=0; i < list.size(); i++) {
            Expression expr = list.getElement(i);
            if (expr instanceof ListExpr) {
                ListExpr exprList = (ListExpr) expr;
                if (exprList.size() > 0) {
                    Expression first = exprList.getElement(0);
                    if (first instanceof SymbolExpr) {
                        String symbol = ((SymbolExpr)first).value;
                        if (symbol.equals("unquote")) {
                            Expression unquoted = exprList.getElement(1);
                            if (unquoted instanceof ListExpr) {
                                matches.add(parseSexprMatchVariable((ListExpr) unquoted));
                                continue;
                            } else if (unquoted instanceof SymbolExpr) {
                                matches.add(new MatchVariable(((SymbolExpr)unquoted).value));
                                continue;
                            }
                        } else if (symbol.equals("unquote-splice")) {
                            Expression unquoted = exprList.getElement(1);
                            if (unquoted instanceof ListExpr) {
                                throw new LispException("Cannot match ,@ followed by a list");
                            } else if (unquoted instanceof SymbolExpr) {
                                if (i < list.size() - 1) {
                                    throw new LispException("Can only match ,@ as the last item in a list");
                                }
                                matches.add(new MatchVariable(((SymbolExpr)unquoted).value));
                                lastIsSplice = true;
                                continue;
                            }
                        }
                    }
                }
                matches.add(quotedToMatchPatterns(exprList));
            } else if (expr instanceof BoolExpr) {
                matches.add(new MatchValueConstructor("SexprBool",
                        Arrays.asList(new MatchBool(((BoolExpr)expr).value))));
            } else if (expr instanceof CharExpr) {
                matches.add(new MatchValueConstructor("SexprChar",
                        Arrays.asList(new MatchChar(((CharExpr)expr).value))));
            } else if (expr instanceof IntExpr) {
                matches.add(new MatchValueConstructor("SexprInt",
                        Arrays.asList(new MatchInt(((IntExpr)expr).value))));
            } else if (expr instanceof DoubleExpr) {
                matches.add(new MatchValueConstructor("SexprDouble",
                        Arrays.asList(new MatchDouble(((DoubleExpr)expr).value))));
            } else if (expr instanceof StringExpr) {
                matches.add(new MatchValueConstructor("SexprString",
                        Arrays.asList(new MatchString(((StringExpr)expr).value))));
            } else if (expr instanceof SymbolExpr) {
                String symbol = ((SymbolExpr)expr).value;
                if (symbol.equals("_")) {
                    matches.add(new MatchVariable(symbol));
                } else {
                    matches.add(new MatchValueConstructor("SexprSymbol",
                            Arrays.asList(new MatchSymbol((SymbolExpr) expr))));
                }
            } else {
                throw new LispException("Can't match expression "+expr+" in S-expression");
            }
        }

        Match curr;
        int start;

        if (!lastIsSplice) {
            curr = new MatchValueConstructor("Nil", new ArrayList<>());
            start = matches.size() - 1;
        } else {
            curr = matches.get(matches.size()-1);
            start = matches.size() - 2;
        }
        for (int i=start; i >= 0; i--) {
            Match match = matches.get(i);
            curr = new MatchValueConstructor("Cons", Arrays.asList(match, curr));
        }
        return new MatchValueConstructor("SexprList", Arrays.asList(curr));
    }

    protected Match parseSexprMatchVariable(ListExpr matchVariable) throws LispException {
        if (matchVariable.size() != 2) {
            throw new LispException("S-expression typed-match should contain a type and a variable");
        }
        Expression typeExpr = matchVariable.getElement(0);
        if (!(typeExpr instanceof SymbolExpr)) {
            throw new LispException("S-expression typed match type should be a symbol (bool, int, char, etc.)");
        }
        String typeStr = ((SymbolExpr)typeExpr).value;

        Expression varExpr = matchVariable.getElement(1);
        if (!(varExpr instanceof SymbolExpr)) {
            throw new LispException("S-expression typed match variable name should be a symbol");
        }
        String varStr = ((SymbolExpr)varExpr).value;

        if (typeStr.equals("bool") || typeStr.equals("SexprBool")) {
            return new MatchValueConstructor("SexprBool", Arrays.asList(new MatchVariable(varStr)));
        } else if (typeStr.equals("char") || typeStr.equals("SexprChar")) {
            return new MatchValueConstructor("SexprChar", Arrays.asList(new MatchVariable(varStr)));
        } else if (typeStr.equals("int") || typeStr.equals("SexprInt")) {
            return new MatchValueConstructor("SexprInt", Arrays.asList(new MatchVariable(varStr)));
        } else if (typeStr.equals("double") || typeStr.equals("SexprDouble")) {
            return new MatchValueConstructor("SexprDouble", Arrays.asList(new MatchVariable(varStr)));
        } else if (typeStr.equals("string") || typeStr.equals("SexprString")) {
            return new MatchValueConstructor("SexprString", Arrays.asList(new MatchVariable(varStr)));
        } else if (typeStr.equals("symbol") || typeStr.equals("SexprSymbol")) {
            return new MatchValueConstructor("SexprSymbol", Arrays.asList(new MatchVariable(varStr)));
        } else if (typeStr.equals("list") || typeStr.equals("SexprList")) {
            return new MatchValueConstructor("SexprList", Arrays.asList(new MatchVariable(varStr)));
        } else {
            throw new LispException("Invalid S-expression type name "+typeStr);
        }
    }

    protected static final Set<String> sexprTypeNames = new HashSet<>(
            Arrays.asList("bool", "char", "int", "double", "string", "symbol", "list"));
    protected boolean isSexprTypeName(String str) {
        return sexprTypeNames.contains(str);
    }
}

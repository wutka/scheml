package edu.vanderbilt.cs.wutkam.scheme.expr.match;

import edu.vanderbilt.cs.wutkam.scheme.expr.TypeConstructorExpr;
import edu.vanderbilt.cs.wutkam.scheme.runtime.SchemeRuntime;
import edu.vanderbilt.cs.wutkam.scheme.type.AbstractTypeDecl;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

/**
 */
public class ExhaustivenessChecker {
    public void checkExhaustiveness(List<Match> patterns) {
        List<Stack<Match>> patternMatrix = new ArrayList<>();
        for (Match match: patterns) {
            Stack<Match> newStack = new Stack<>();
            newStack.push(match);
            patternMatrix.add(newStack);
        }

        for (int i=1; i < patterns.size(); i++) {
            List<Stack<Match>> patternSubmatrix = patternMatrix.subList(0, i);
            Stack<Match> usefulness = checkUsefulness(patternSubmatrix, patternMatrix.get(i));
            if (usefulness.isEmpty()) {
                SchemeRuntime.warn("Pattern "+patterns.get(i).toString()+" is redundant");
            }
        }
        Stack<Match> exhaustiveStack = new Stack<>();
        exhaustiveStack.push(new MatchVariable("_"));

        Stack<Match> exhaustive = checkUsefulness(patternMatrix, exhaustiveStack);
        if (!exhaustive.isEmpty()) {
            SchemeRuntime.warn("Pattern match is not exhaustive, an unmatched pattern is "+
                    exhaustive.pop().toString());
        }
    }

    public Stack<Match> checkUsefulness(List<Stack<Match>> patternMatrix, Stack<Match> q) {
        if (q.isEmpty()) return new Stack<>();

        Match matchType = findExampleType(patternStack, q);

        if (matchType instanceof MatchVariable) {
            Stack<Match> returnVal = new Stack<>();
            returnVal.push(new MatchVariable("_"));
        } else if (matchType instanceof MatchTypeConstructor) {
            MatchTypeConstructor matchTypeConstructor = (MatchTypeConstructor) matchType;

            Match q1 = q.pop();
            if (q1 instanceof MatchVariable) {

            }
        }
    }

    public List<Stack<Match>> specialize(Match constructor, Stack<Match> currStack) {
        List<Stack<Match>> result = new ArrayList<>();

        Match pattern = currStack.pop();

        if (constructor instanceof MatchTypeConstructor) {
            MatchTypeConstructor matchCons = (MatchTypeConstructor) constructor;
            if (pattern instanceof MatchTypeConstructor) {
                MatchTypeConstructor pattCons = (MatchTypeConstructor) pattern;
                if (!matchCons.equals(pattCons)) {
                    return result;
                }
                for (int i= pattCons.targetPatterns.size(); i >= 0; i--) {
                    currStack.push(pattCons.targetPatterns.get(i));
                }
                result.add(currStack);
            } else if (pattern instanceof MatchVariable) {

            }

        }
    }
    protected List<Match> getAllConstructorMatchers(String constructorName) {
        AbstractTypeDecl abstractTypeDecl = SchemeRuntime.getTypeRegistry().findByConstructor(constructorName);
        List<Match> retval = new ArrayList<>();
        for (TypeConstructorExpr typeConstructor: abstractTypeDecl.typeConstructors.values()) {
            List<Match> wildcards = new ArrayList<>();
            for (int i=0; i < typeConstructor.paramTypes.length; i++) {
                wildcards.add(new MatchVariable("_"));
            }
            retval.add(new MatchTypeConstructor(typeConstructor.name, wildcards));
        }
        return retval;
    }

    protected Match findExampleType(List<Stack<Match>> patternStack, Stack<Match> q) {
        if (!(q.peek() instanceof MatchVariable)) return q.peek();
        for (Stack<Match> p: patternStack) {
            if (!(p.peek() instanceof MatchVariable)) return p.peek();
        }
        return new MatchVariable("_");
    }
}

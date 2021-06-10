package edu.vanderbilt.cs.wutkam.scheme.expr.match;

import edu.vanderbilt.cs.wutkam.scheme.expr.TypeConstructorExpr;
import edu.vanderbilt.cs.wutkam.scheme.runtime.SchemeRuntime;
import edu.vanderbilt.cs.wutkam.scheme.type.AbstractTypeDecl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Collectors;

/**
 */
public class ExhaustivenessChecker {
    public static final MatchVariable WILDCARD = new MatchVariable("_");
    public static void checkExhaustiveness(List<Match> patterns) {

        List<Stack<Match>> patternMatrix = new ArrayList<>();
        Stack<Match> stack = new Stack<>();
        stack.push(patterns.get(0));
        patternMatrix.add(stack);
        if (patterns.size() > 0) {
            for (int i=1; i < patterns.size(); i++) {
                Stack<Match> qpStack = new Stack<>();
                qpStack.push(patterns.get(i));
                Stack<Match> usefulness = checkUsefulness(patternMatrix, qpStack);
                if (usefulness == null) {
                    SchemeRuntime.warn("Pattern " + patterns.get(i).toString() + " is redundant");
                }
                Stack<Match> pattStack = new Stack<>();
                pattStack.push(patterns.get(i));
                patternMatrix.add(pattStack);
            }
        }

        Stack<Match> wildcardStack = new Stack<>();
        wildcardStack.push(WILDCARD);
        Stack<Match> exhaustive = checkUsefulness(patternMatrix, wildcardStack);
        if (exhaustive != null) {
            SchemeRuntime.warn("Pattern match is not exhaustive, an unmatched pattern is "+
                    exhaustive.pop().toString());
        }
    }

    public static Stack<Match> checkUsefulness(List<Stack<Match>> patternMatrix, Stack<Match> qStack) {
        if (qStack.isEmpty()) {
            if (patternMatrix.isEmpty()) {
                return new Stack<>();
            } else {
                return null;
            }
        }

        Match q = qStack.pop();
        if (q instanceof MatchTypeConstructor) {
            MatchTypeConstructor qMatchCons = (MatchTypeConstructor) q;
            for (int i=qMatchCons.targetPatterns.size()-1; i >= 0; i--) {
                qStack.push(qMatchCons.targetPatterns.get(i));
            }
            List<Stack<Match>> submatrix = getPatternsWithConstructor(qMatchCons, patternMatrix);
            for (Stack<Match> subPatt: submatrix) {
                MatchTypeConstructor subPattCons = (MatchTypeConstructor) subPatt.pop();
                for (int i=subPattCons.targetPatterns.size()-1; i >= 0; i--) {
                    subPatt.push(subPattCons.targetPatterns.get(i));
                }
            }
            Stack<Match> matched = checkUsefulness(submatrix, qStack);
            if (matched == null) {
                return null;
            }
            List<Match> typeCons = new ArrayList<>();
            for (int i=0; i < qMatchCons.targetPatterns.size(); i++) {
                typeCons.add(matched.pop());
            }
            MatchTypeConstructor resultMatch = new MatchTypeConstructor(qMatchCons.constructorName, typeCons);
            matched.push(resultMatch);
            return matched;
        } else if (q instanceof MatchVariable) {
            Match matchType = findExampleType(patternMatrix, q);

            if (matchType instanceof MatchTypeConstructor) {
                MatchTypeConstructor matchTypeConstructor = (MatchTypeConstructor) matchType;

                List<Match> matchers = getAllConstructorMatchers(matchTypeConstructor.constructorName);
                for (Match match: matchers) {
                    qStack.push(match);
                    Stack<Match> result = checkUsefulness(patternMatrix, qStack);
                    if (result != null) {
                        return result;
                    }
                    qStack.pop();
                }
                return null;
            } else if ((matchType instanceof MatchBool) || (matchType instanceof MatchChar) ||
                       (matchType instanceof MatchDouble) || (matchType instanceof MatchInt) ||
                       (matchType instanceof MatchString)) {
                List<Match> matchers = findPossibleValues(patternMatrix, matchType);
                for (Match match : matchers) {
                    qStack.push(match);
                    List<Stack<Match>> subpatterns = new ArrayList<>();
                    for (Stack<Match> pattern : patternMatrix) {
                        if (pattern.peek().equals(match)) {
                            subpatterns.add(pattern);
                        }
                    }
                    Stack<Match> result = checkUsefulness(subpatterns, qStack);
                    if (result != null) {
                        return result;
                    }
                    qStack.pop();
                }
                return null;
            } else {
                return null;
            }
        } else {
            List<Stack<Match>> subpatterns = new ArrayList<>();
            for (Stack<Match> pattern: patternMatrix) {
                if (q.equals(pattern.peek())) {
                    pattern.pop();
                    subpatterns.add(pattern);
                }
            }
            Stack<Match> result = checkUsefulness(subpatterns, qStack);
            if (result != null) {
                result.push(q);
            }
            return result;
        }
    }

    public static final String[] tryStrings = new String[] { "foo", "bar", "baz", "quux", "qux", "corge", "grault",
            "garply", "waldo", "fred", "plugh", "xyxxy", "thud" };

    protected static List<Match> findPossibleValues(List<Stack<Match>> patternMatrix, Match matchType) {
        Set<Match> values = new HashSet<>();
        for (Stack<Match> pattern: patternMatrix) {
            values.add(pattern.peek());
        }
        Match missingValue = findMissingValue(values, matchType);
        if (missingValue != null) {
            values.add(missingValue);
        }
        return new ArrayList<>(values);
    }

    protected static Match findMissingValue(Set<Match> values, Match matchType) {
        if (matchType instanceof MatchTypeConstructor) {
            List<MatchString> constructors = getAllConstructorNames(
                    ((MatchTypeConstructor) matchType).constructorName);
            for (MatchString con: constructors) {
                if (!values.contains(con)) return con;
            }
            return null;
        } else if (matchType instanceof MatchBool) {
            if (values.size() == 2) return null;
            return new MatchBool(values.contains(new MatchBool(false)));
        } else if (matchType instanceof MatchChar) {
            List<Character> chars = values.stream().filter(m -> m instanceof MatchChar).
                    map(mc -> ((MatchChar)mc).value).collect(Collectors.toList());
            Collections.sort(chars);
            if (chars.size() == 0) return new MatchChar('A');
            if (chars.size() == 1) return new MatchChar((char) (chars.get(0).charValue()+1));
            char ch1 = chars.get(0);
            for (int i=1; i < chars.size(); i++) {
                char ch2 = chars.get(i);
                if (ch2 - ch1 > 1) {
                    return new MatchChar((char) (ch1 + 1));
                }
                ch1 = ch2;
            }
            return new MatchChar((char) (ch1 + 1));
        } else if (matchType instanceof MatchDouble) {
            List<Double> doubles = values.stream().filter(m -> m instanceof MatchDouble).
                    map(md -> ((MatchDouble) md).value).collect(Collectors.toList());

            Collections.sort(doubles);
            if (doubles.size() == 0) return new MatchDouble(0.0);
            if (doubles.size() == 1) return new MatchDouble(doubles.get(0) + 1.0);
            double d1 = doubles.get(0);
            for (int i = 1; i < doubles.size(); i++) {
                double d2 = doubles.get(i);
                if (d2 - d1 > 1.0) {
                    return new MatchDouble(d1 + 1.0);
                }
                d1 = d2;
            }
            return new MatchDouble(d1 + 1.0);
        } else if (matchType instanceof MatchInt) {
            List<Integer> ints = values.stream().filter(m -> m instanceof MatchInt).
                    map(mi -> ((MatchInt) mi).value).collect(Collectors.toList());

            Collections.sort(ints);
            if (ints.size() == 0) return new MatchInt(0);
            if (ints.size() == 1) return new MatchInt(ints.get(0) + 1);
            int i1 = ints.get(0);
            for (int i = 1; i < ints.size(); i++) {
                int i2 = ints.get(i);
                if (i2 - i1 > 1.0) {
                    return new MatchInt(i1 + 1);
                }
                i1 = i2;
            }
            return new MatchInt(i1 + 1);
        } else if (matchType instanceof MatchString) {
            List<String> strings = values.stream().filter(m -> m instanceof MatchString).
                    map(ms -> ((MatchString) ms).value).collect(Collectors.toList());

            if (strings.size() == 0) return new MatchString(tryStrings[0]);

            for (int i=0; i < tryStrings.length; i++) {
               if (!(strings.contains(tryStrings[i]))) return new MatchString(tryStrings[i]);
            }

            return new MatchString(strings.get(strings.size()-1)+"A");
        }
        return null;

    }

    public static List<Stack<Match>> getPatternsWithConstructor(MatchTypeConstructor matchTypeCons,
                                                                List<Stack<Match>> patternMatrix) {
        List<Stack<Match>> retval = new ArrayList<>();

        for (Stack<Match> pattStack: patternMatrix) {
            Match m = pattStack.peek();
            if (m instanceof MatchVariable) {
                Stack<Match> newStack = new Stack<>();
                newStack.addAll(pattStack);
                newStack.pop();
                newStack.push(getWildcardMatcher(matchTypeCons));
                retval.add(newStack);
            } else if (m instanceof MatchTypeConstructor) {
                MatchTypeConstructor pattCons = (MatchTypeConstructor) m;
                if (pattCons.constructorName.equals(matchTypeCons.constructorName)) {
                    retval.add(pattStack);
                }
            }
        }
        return retval;
    }

    public static Map<Match,List<Match>> partitionPatterns(List<Match> patterns, Match matchType) {
        Map<Match,List<Match>> retval = new HashMap<>();

        for (Match pattern: patterns) {
            Match key;
            if (pattern instanceof MatchTypeConstructor) {
                key = new MatchString(((MatchTypeConstructor) pattern).constructorName);
            } else if (pattern instanceof MatchVariable) {
                key = WILDCARD;
            } else {
                key = pattern;
            }
            List<Match> values = retval.get(key);
            if (values == null) values = new ArrayList<>();
            values.add(pattern);
            retval.put(key, values);
        }
        return retval;
    }

    protected static List<MatchString> getAllConstructorNames(String constructorName) {
        AbstractTypeDecl abstractTypeDecl = SchemeRuntime.getTypeRegistry().findByConstructor(constructorName);
        List<MatchString> retval = new ArrayList<>();
        for (TypeConstructorExpr typeConstructor: abstractTypeDecl.typeConstructors.values()) {
            retval.add(new MatchString(typeConstructor.name));
        }
        return retval;
    }

    protected static MatchTypeConstructor getWildcardMatcher(MatchTypeConstructor type) {
        List<Match> targetPatterns = new ArrayList<>();
        for (int i=0; i < type.targetPatterns.size(); i++) targetPatterns.add(WILDCARD);
        return new MatchTypeConstructor(type.constructorName, targetPatterns);
    }

    protected static List<Match> getAllConstructorMatchers(String constructorName) {
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

    protected static Match findExampleType(List<Stack<Match>> patternStack, Match q) {
        if (!(q instanceof MatchVariable)) return q;
        for (Stack<Match> p: patternStack) {
            if (!(p.peek() instanceof MatchVariable)) return p.peek();
        }
        return new MatchVariable("_");
    }

    static abstract class UsefulnessResult {

    }

    static class UsefulnessCounterexample extends UsefulnessResult {
        public Match value;

        public UsefulnessCounterexample(Match value) {
            this.value = value;
        }
    }

    static class UsefulnessRemainingPatterns extends UsefulnessResult {
        public Match matchValue;
        public List<Match> remainingPatterns;

        public UsefulnessRemainingPatterns(Match matchValue, List<Match> remainingPatterns) {
            this.remainingPatterns = remainingPatterns;
        }
    }
}

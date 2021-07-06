package edu.vanderbilt.cs.wutkam.scheml.expr.match;

import edu.vanderbilt.cs.wutkam.scheml.expr.ValueConstructorExpr;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;
import edu.vanderbilt.cs.wutkam.scheml.type.AbstractTypeDecl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Collectors;

/** Implements usefulness/exhaustiveness checking using the algorithm described by Luc Maranget in
 * "Warnings for Pattern Matching" http://pauillac.inria.fr/~maranget/papers/warn/warn.pdf
 *
 * Usefulness indicates whether a pattern matches something that the previous patterns don't
 * match. To compute exhaustiveness, we just compute the usefulness of adding a "_" match to
 * the end of the pattern list.
 */
public class ExhaustivenessChecker {
    // We use wildcards a lot and we can just reuse this instance
    public static final MatchVariable WILDCARD = new MatchVariable("_");

    /** Checks the exhaustiveness and usefulness (is it redundant or not) for a list
     * of patterns from a match.
     */
    public static void checkExhaustiveness(List<Match> patterns) {
        // A list of patterns is referred to a matrix in the Maranget paper
        List<Stack<Match>> patternMatrix = new ArrayList<>();

        // Each pattern is stored as a stack, initially this will be a single
        // object since (match) just matches against single objects, but when the
        // item is a constructed type it will be popped off the stack and replaced
        // with each item that makes up the constructed type
        Stack<Match> stack = new Stack<>();
        stack.push(patterns.get(0));
        patternMatrix.add(stack);

        // We start with one pattern on the stack, and if there are more than one pattern,
        // we check each pattern's usefulness against the patterns ahead of it. If a pattern
        // has no usefulness, it is redundant and we display a warning
        for (int i=1; i < patterns.size(); i++) {
            // Create a stack to use this pattern as the q argument to the usefulness call
            Stack<Match> qpStack = new Stack<>();
            qpStack.push(patterns.get(i));

            // Compute the usefulness of this pattern
            // We copy the pattern matrix here because you should not call checkUsefulness on the
            // same matrix or q stack twice because it will pop items off those stacks
            Stack<Match> usefulness = checkUsefulness(copyPatternMatrix(patternMatrix), qpStack);

            // If it is null, then there was no pattern that this new pattern could match that
            // the previous ones didn't already match, so flag a warning
            if (usefulness == null) {
                SchemlRuntime.warn("Pattern " + patterns.get(i).toString() + " is redundant");
            }
            // Go ahead and add it to the pattern matrix anyway
            Stack<Match> pattStack = new Stack<>();
            pattStack.push(patterns.get(i));
            patternMatrix.add(pattStack);
        }

        // Re-create the whole pattern matrix since we need a fresh copy
        patternMatrix = new ArrayList<>();
        for (Match pattern: patterns) {
            Stack<Match> pattStack = new Stack<>();
            pattStack.push(pattern);
            patternMatrix.add(pattStack);
        }

        // Compute the exhaustiveness of the match by seeing if a wildcard match is useful. If it is,
        // we get back a pattern that is an example of what it matched that the other patterns didn't,
        // and we display it as a warning. If the wildcard is not useful, then the existing pattern list
        // covers all possibilities
        Stack<Match> wildcardStack = new Stack<>();
        wildcardStack.push(WILDCARD);
        Stack<Match> exhaustive = checkUsefulness(patternMatrix, wildcardStack);

        if (exhaustive != null) {
            SchemlRuntime.warn("Pattern match is not exhaustive, an unmatched pattern is "+
                    exhaustive.pop().toString());
        }
    }

    /** Determines whether the q (qStack) argument is useful against an existing patternMatrix. That is,
     * does it match anything that the existing pattern matrix doesn't.
     */
    public static Stack<Match> checkUsefulness(List<Stack<Match>> patternMatrix, Stack<Match> qStack) {
        // If qStack is empty then we have tried to match every piece of it
        if (qStack.isEmpty()) {
            // If the pattern matrix is also empty, then by the time we got to the end of q, there
            // were no patterns that still matched it, so return a non-null result that will be built up
            // into the example pattern of what it matched
            if (patternMatrix.isEmpty()) {
                return new Stack<>();
            } else {
                // Otherwise, if the pattern matrix is not empty, then when we got to the end of q, it had
                // matched one or more patterns all the way to the end, so it is not useful
                return null;
            }
        }

        // Pop the next item off the q stack
        Match q = qStack.pop();

        if (q instanceof MatchValueConstructor) {

            // If q is a match constructor, push all its target patterns (the individual fields of the
            // constructed item) onto the stack
            MatchValueConstructor qMatchCons = (MatchValueConstructor) q;
            for (int i=qMatchCons.targetPatterns.size()-1; i >= 0; i--) {
                qStack.push(qMatchCons.targetPatterns.get(i));
            }

            // Find all the patterns in the pattern matrix that match q's constructor. For instance, if
            // q is some kind of (Cons x ...), then patterns in the matrix that are Nil get filtered out
            // because they can't match q
            List<Stack<Match>> submatrix = getPatternsWithConstructor(qMatchCons, patternMatrix);

            // For each pattern in the matrix, pop off the constructor and push its target patterns just as
            // was done with q, so q and each pattern in the matrix should be the same length
            for (Stack<Match> subPatt: submatrix) {
                MatchValueConstructor subPattCons = (MatchValueConstructor) subPatt.pop();
                for (int i=subPattCons.targetPatterns.size()-1; i >= 0; i--) {
                    subPatt.push(subPattCons.targetPatterns.get(i));
                }
            }

            // Recursively check the usefulness of the current submatrix and q stack
            Stack<Match> matched = checkUsefulness(submatrix, qStack);

            // If q failed to match any of these subpatterns, return null
            if (matched == null) {
                return null;
            }

            // Otherwise, since Q did match something, the matched stack should contain a match for
            // each targetPattern in Q. Re-create the value constructor for Q by popping its target patterns
            // off the stack
            List<Match> valueCons = new ArrayList<>();
            for (int i=0; i < qMatchCons.targetPatterns.size(); i++) {
                valueCons.add(matched.pop());
            }

            // ... and then instantiate an instance of q's value constructor
            MatchValueConstructor resultMatch = new MatchValueConstructor(qMatchCons.constructorName, valueCons);

            // and push that back onto the result match
            matched.push(resultMatch);
            return matched;
        } else if (q instanceof MatchVariable) {

            // If q was a match variable, try to figure out what types we are matching by looking at the pattern
            // matrix and seeing if any of the first items in each of those stacks is not a match variable
            Match matchType = findExampleType(patternMatrix, q);

            if (matchType instanceof MatchValueConstructor) {
                MatchValueConstructor matchValueConstructor = (MatchValueConstructor) matchType;

                // If q is a match variable and is matching against some kind of constructed type, get a list
                // of all the constructors for that constructed type
                List<Match> matchers = getAllConstructorMatchers(matchValueConstructor.constructorName);

                // We will try matching q against each type of constructor
                for (Match match: matchers) {
                    // Try matching against the next constructor type
                    qStack.push(match);

                    // Make a copy of the pattern matrix because we are looping here and checkUsefulness will pop
                    // values off the pattern matrix
                    List<Stack<Match>> tempMatrix = copyPatternMatrix(patternMatrix);

                    // See if q being this type of constructor is useful
                    Stack<Match> result = checkUsefulness(tempMatrix, copyStack(qStack));
                    qStack.pop();

                    // If we got a useful result for this constructor type, return it
                    if (result != null) {
                        return result;
                    }
                    // otherwise we'll loop back and try the next constructor type
                }
                // If we get here none of the constructor types were useful
                return null;
            } else if ((matchType instanceof MatchBool) || (matchType instanceof MatchChar) ||
                       (matchType instanceof MatchDouble) || (matchType instanceof MatchInt) ||
                       (matchType instanceof MatchString)) {

                // q is a wildcard, being matched against a simple type. Other than bool, which in other languages
                // is implemented as a constructed type, we assume that the match expression does not completely
                // enumerate all the possible values (the smallest value here is char and in Java that's 65,536
                // possible values
                // See the description of findPossibleValues for the algorithm determining the possible match values
                List<Match> matchers = findPossibleValues(patternMatrix, matchType);

                for (Match match : matchers) {
                    qStack.push(match);
                    List<Stack<Match>> subpatterns = new ArrayList<>();
                    // Since we are trying multiple options, make a copy of the matrix each time through
                    List<Stack<Match>> tempMatrix = copyPatternMatrix(patternMatrix);

                    // Since q is a simple variable, look at the value at the top of each pattern stack
                    // the pattern matches if the pattern is a variable or if its value exactly equals q
                    for (Stack<Match> pattern : tempMatrix) {
                        if ((pattern.peek() instanceof MatchVariable) || pattern.peek().equals(match)) {
                            subpatterns.add(pattern);
                        }
                    }
                    // continue the usefulness check
                    Stack<Match> result = checkUsefulness(subpatterns, copyStack(qStack));
                    qStack.pop();

                    // If we found a match return it
                    if (result != null) {
                        return result;
                    }
                }
                // If we get through the loop without matching, return a null to indicate failure to match
                return null;
            } else if (matchType instanceof MatchVariable) {
                // If we try to figure out a type but all the patterns and q are variables,
                // all the patterns match q
                List<Stack<Match>> subpatterns = new ArrayList<>();
                for (Stack<Match> pattern : patternMatrix) {
                    pattern.pop();
                    subpatterns.add(pattern);
                }
                // Just continue with the next match
                Stack<Match> result = checkUsefulness(subpatterns, qStack);
                if (result != null) {
                    // If q is useful, push the value of q onto the result stack
                    // so a useful value of q can be reconstructed
                    result.push(q);
                    return result;
                } else {
                    return null;
                }
            } else {
                // Otherwise we failed to match
                return null;
            }
        } else {
            // If q wasn't a variable or a value constructor, it must be a simple variable
            // so compare it with the top of each pattern stack. If the top of the
            // pattern stack is a variable then consider it to match
            List<Stack<Match>> subpatterns = new ArrayList<>();
            for (Stack<Match> pattern: patternMatrix) {
                if ((pattern.peek() instanceof MatchVariable) || q.equals(pattern.peek())) {
                    pattern.pop();
                    subpatterns.add(pattern);
                }
            }
            Stack<Match> result = checkUsefulness(subpatterns, qStack);
            // If the rest of q was useful, push this value of q onto the result stack
            // for rebuilding the value of q that is useful
            if (result != null) {
                result.push(q);
            }
            return result;
        }
    }

    /** These are the metasyntactic variables from the "See also" for the entry on "foo" in the
     * Jargon file: http://www.catb.org/jargon/html/F/foo.html
     * The entry on metasyntactic variables givs even more, but this seems like a nice set
     */
    public static final String[] tryStrings = new String[] { "foo", "bar", "baz", "quux", "qux", "corge", "grault",
            "garply", "waldo", "fred", "plugh", "xyxxy", "thud" };

    /** Makes a copy of a stack */
    protected static Stack<Match> copyStack(Stack<Match> stack) {
        Stack<Match> newStack = new Stack<>();
        newStack.addAll(stack);
        return newStack;
    }

    /** Makes a copy of the pattern matrix */
    protected static List<Stack<Match>> copyPatternMatrix(List<Stack<Match>> patternMatrix) {
        List<Stack<Match>> newMatrix = new ArrayList<>();
        for (Stack<Match> pattern: patternMatrix) {
            newMatrix.add(copyStack(pattern));
        }
        return newMatrix;
    }

    /** Finds all the possible values for a simple value. We look through the pattern matrix to find all the
     * values that have been used, and then see if we can figure out one that is missing, and return all
     * the ones used plus one of the missing ones.
     * findMissingValue has the logic for choosing missing values for each type
     */
    protected static List<Match> findPossibleValues(List<Stack<Match>> patternMatrix, Match matchType) {
        Set<Match> values = new HashSet<>();
        for (Stack<Match> pattern: patternMatrix) {
            if (!(pattern.peek() instanceof MatchVariable)) {
                values.add(pattern.peek());
            }
        }
        Match missingValue = findMissingValue(values, matchType);
        if (missingValue != null) {
            values.add(missingValue);
        }
        return new ArrayList<>(values);
    }

    /** Given a set of values used in a pattern, and a type, choose a value of
     * that type that has not been matched against.
     */
    protected static Match findMissingValue(Set<Match> values, Match matchType) {
        if (matchType instanceof MatchValueConstructor) {
            // For a value constructor, just get a list of all the constructors
            List<MatchString> constructors = getAllConstructorNames(
                    ((MatchValueConstructor) matchType).constructorName);

            // See if there is one that isn't in the value set
            for (MatchString con: constructors) {
                if (!values.contains(con)) return con;
            }

            // If nothing is missing, return null
            return null;
        } else if (matchType instanceof MatchBool) {
            // If there are two different boolean values, nothing is missing
            if (values.size() == 2) return null;

            // Otherwise if values contains false, return true
            // and if it doesn't contain false, return false
            return new MatchBool(values.contains(new MatchBool(false)));
        } else if (matchType instanceof MatchChar) {
            // Turn the set of MatchChar values into a list of characters
            List<Character> chars = values.stream().filter(m -> m instanceof MatchChar).
                    map(mc -> ((MatchChar)mc).value).collect(Collectors.toList());

            // Sort the list of characters
            Collections.sort(chars);

            // If there are no characters, return 'A'
            if (chars.size() == 0) return new MatchChar('A');

            // If there is one character, return the char after it
            if (chars.size() == 1) return new MatchChar((char) (chars.get(0) +1));

            // Otherwise, iterate through the character values looking for a gap between
            // successive characters and return that
            char ch1 = chars.get(0);
            for (int i=1; i < chars.size(); i++) {
                char ch2 = chars.get(i);
                if (ch2 - ch1 > 1) {
                    return new MatchChar((char) (ch1 + 1));
                }
                ch1 = ch2;
            }
            // Otherwise, return one higher than the highest char
            return new MatchChar((char) (ch1 + 1));
        } else if (matchType instanceof MatchDouble) {
            // Turn the set of MatchDouble values into a list of doubles
            List<Double> doubles = values.stream().filter(m -> m instanceof MatchDouble).
                    map(md -> ((MatchDouble) md).value).collect(Collectors.toList());

            // Sort the list of characters
            Collections.sort(doubles);

            // If there are no double values return 0.0
            if (doubles.size() == 0) return new MatchDouble(0.0);

            // If there is one double value return the value + 1.0
            if (doubles.size() == 1) return new MatchDouble(doubles.get(0) + 1.0);

            // Otherwise loop through the values looking for a gap bigger than 1.0 between
            // variables and return the lower one plus 1.0 (might be better to just take
            // the average of the first two)
            double d1 = doubles.get(0);
            for (int i = 1; i < doubles.size(); i++) {
                double d2 = doubles.get(i);
                if (d2 - d1 > 1.0) {
                    return new MatchDouble(d1 + 1.0);
                }
                d1 = d2;
            }
            // Otherwise return the highest value plus 1.0
            return new MatchDouble(d1 + 1.0);
        } else if (matchType instanceof MatchInt) {
            // Turn the set of MatchInt values into a list of longs
            List<Long> ints = values.stream().filter(m -> m instanceof MatchInt).
                    map(mi -> ((MatchInt) mi).value).collect(Collectors.toList());

            // Sort the values
            Collections.sort(ints);

            // If there are no values, return 0
            if (ints.size() == 0) return new MatchInt(0);

            // If there is one value, return the value plus 1
            if (ints.size() == 1) return new MatchInt(ints.get(0) + 1);

            // Otherwise loop through the values looking for a gap between consecutive
            // values and return 1+the lower value
            long i1 = ints.get(0);
            for (int i = 1; i < ints.size(); i++) {
                long i2 = ints.get(i);
                if (i2 - i1 > 1.0) {
                    return new MatchInt(i1 + 1);
                }
                i1 = i2;
            }
            // Otherwise return the highest value + 1
            return new MatchInt(i1 + 1);
        } else if (matchType instanceof MatchString) {
            // Turn the list of MatchString values into a list of strings
            List<String> strings = values.stream().filter(m -> m instanceof MatchString).
                    map(ms -> ((MatchString) ms).value).collect(Collectors.toList());

            // Sort the values
            Collections.sort(strings);

            // If there are no strings, return the first of the pre-configured tryStrings
            if (strings.size() == 0) return new MatchString(tryStrings[0]);

            // Otherwise, see if there are any tryStrings that aren't in the list of values
            for (int i=0; i < tryStrings.length; i++) {
               if (!(strings.contains(tryStrings[i]))) return new MatchString(tryStrings[i]);
            }

            // In the unlikely case that all the tryStrings are matched, just append an A
            // to the highest string value
            return new MatchString(strings.get(strings.size()-1)+"A");
        }
        return null;

    }

    /** Returns the subset of the patternMatrix that starts with the given value constructor */
    public static List<Stack<Match>> getPatternsWithConstructor(MatchValueConstructor matchValueCons,
                                                                List<Stack<Match>> patternMatrix) {
        List<Stack<Match>> retval = new ArrayList<>();

        // Loop through the matrix
        for (Stack<Match> pattStack: patternMatrix) {
            Match m = pattStack.peek();
            if (m instanceof MatchVariable) {
                // If the entry in the stack is a variable, replace it with the value constructor
                // with _ for every component of the constructor
                Stack<Match> newStack = new Stack<>();
                newStack.addAll(pattStack);
                newStack.pop();
                newStack.push(getWildcardMatcher(matchValueCons));
                retval.add(newStack);
            } else if (m instanceof MatchValueConstructor) {
                // If the pattern has a value constructor, add it if it is the same constructor
                MatchValueConstructor pattCons = (MatchValueConstructor) m;
                if (pattCons.constructorName.equals(matchValueCons.constructorName)) {
                    retval.add(pattStack);
                }
            }
        }
        return retval;
    }

    /** Returns all the constructor for the name of the type that includes the given constructor name */
    protected static List<MatchString> getAllConstructorNames(String constructorName) {
        // Find the type that this constructor generates
        AbstractTypeDecl abstractTypeDecl = SchemlRuntime.getTypeRegistry().findByConstructor(constructorName);
        List<MatchString> retval = new ArrayList<>();

        // Add all the constructor names for that type
        for (ValueConstructorExpr valueConstructor: abstractTypeDecl.valueConstructors.values()) {
            retval.add(new MatchString(valueConstructor.name));
        }
        return retval;
    }

    /** Returns a MatchValueConstructor for the given type where every target pattern is a wildcard */
    protected static MatchValueConstructor getWildcardMatcher(MatchValueConstructor type) {
        List<Match> targetPatterns = new ArrayList<>();
        for (int i=0; i < type.targetPatterns.size(); i++) targetPatterns.add(WILDCARD);
        return new MatchValueConstructor(type.constructorName, targetPatterns);
    }

    /** Get a list of wildcard matches for all the constructors of a particular type */
    protected static List<Match> getAllConstructorMatchers(String constructorName) {
        // Find the type that includes this constructor
        AbstractTypeDecl abstractTypeDecl = SchemlRuntime.getTypeRegistry().findByConstructor(constructorName);
        List<Match> retval = new ArrayList<>();

        // Loop through all the constructors for this type
        for (ValueConstructorExpr valueConstructor: abstractTypeDecl.valueConstructors.values()) {
            List<Match> wildcards = new ArrayList<>();

            // Set each target pattern to a wildcard
            for (int i=0; i < valueConstructor.paramTypes.length; i++) {
                wildcards.add(WILDCARD);
            }
            retval.add(new MatchValueConstructor(valueConstructor.name, wildcards));
        }
        return retval;
    }

    /** Try to figure out what type q and the top of each pattern stack is based on whether
     * there is a non-variable value in any of them.
     */
    protected static Match findExampleType(List<Stack<Match>> patternStack, Match q) {
        if (!(q instanceof MatchVariable)) return q;
        for (Stack<Match> p: patternStack) {
            if (!(p.peek() instanceof MatchVariable)) return p.peek();
        }
        return WILDCARD;
    }
}

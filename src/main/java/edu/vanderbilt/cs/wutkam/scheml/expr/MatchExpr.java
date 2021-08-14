package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.match.ExhaustivenessChecker;
import edu.vanderbilt.cs.wutkam.scheml.expr.match.Match;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;

import java.util.ArrayList;
import java.util.List;

/** A match expression that evaluates an expression and matches it against a list of patterns
 */
public class MatchExpr implements Expression {
    public final Expression matchExpression;
    public final List<MatchPatternAndTarget> matchPatterns;
    protected boolean exhaustivenessChecked;

    public MatchExpr(Expression matchExpression, List<MatchPatternAndTarget> matchPatterns) {
        this.matchExpression = matchExpression;
        this.matchPatterns = matchPatterns;
        this.exhaustivenessChecked = false;
    }

    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        checkExhaustiveness();

        // Evaluate the expression being matched
        Expression matchAgainst = matchExpression.evaluate(env, false);

        for (MatchPatternAndTarget patternAndTarget: matchPatterns) {
            // Look for the pattern whose constructor name matches that of the expression being matched
            if (patternAndTarget.pattern.matches(matchAgainst)) {
                Environment<Expression> patternEnv = new Environment<>(env);
                patternAndTarget.pattern.defineEnvironment(matchAgainst, patternEnv);
                return patternAndTarget.targetExpression.evaluate(patternEnv, inTailPosition);
            }
        }
        throw new LispException("Pattern match exhausted");
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        checkExhaustiveness();

        TypeRef matchExpressionType = new TypeRef();
        matchExpression.unify(matchExpressionType, env);

        for (MatchPatternAndTarget patternAndTarget: matchPatterns) {
            Environment<TypeRef> matchEnv = new Environment<>(env);
            patternAndTarget.pattern.unify(matchExpressionType, matchEnv);
            patternAndTarget.targetExpression.unify(typeRef, matchEnv);
        }
    }

    @Override
    public Expression toScheml() {
        List<Expression> scheml = new ArrayList<>();
        scheml.add(new SymbolLiteralExpr("match"));
        scheml.add(matchExpression.toScheml());
        for (MatchPatternAndTarget patternAndTarget: matchPatterns) {
            scheml.add(patternAndTarget.toScheml());
        }
        return new ListExpr(scheml);
    }

    public void checkExhaustiveness() {
        if (!exhaustivenessChecked) {
            List<Match> patternList = new ArrayList<>();
            for (MatchExpr.MatchPatternAndTarget pattern: matchPatterns) {
                patternList.add(pattern.pattern);
            }
            ExhaustivenessChecker.checkExhaustiveness(patternList);
            exhaustivenessChecked = true;
        }
    }
    public static class MatchPatternAndTarget {
        public final Match pattern;
        public final Expression targetExpression;

        public MatchPatternAndTarget(Match pattern, Expression targetExpression) {
            this.pattern = pattern;
            this.targetExpression = targetExpression;
        }

        public Expression toScheml() {
            List<Expression> scheml = new ArrayList<>();
            scheml.add(pattern.toScheml());
            scheml.add(targetExpression.toScheml());
            return new ListExpr(scheml);
        }
    }
}

package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.BooleanType;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheml.type.UnifyException;

/** Represents an if expression that has both a true option and false option which must be of
 * the same type, as well as a test that must be boolean.
 */
public class IfExpr implements Expression {
    public final Expression test;
    public final Expression trueOption;
    public final Expression falseOption;

    public IfExpr(Expression test, Expression trueOption, Expression falseOption)
    {
        this.test = test;
        this.trueOption = trueOption;
        this.falseOption = falseOption;
    }

    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        Expression testResult = test.evaluate(env, false);
        if (testResult instanceof BoolExpr) {
            // If the if expression is in the tail position then so are trueOption and falseOption
            if (((BoolExpr)testResult).value) {
                return trueOption.evaluate(env, inTailPosition);
            } else {
                return falseOption.evaluate(env, inTailPosition);
            }
        } else {
            throw new LispException("If expression test did not evaluate to a boolean");
        }
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        // Unify the test with boolean
        TypeRef testType = new TypeRef(BooleanType.TYPE);
        try {
            test.unify(testType, env);
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Test for if expression must be a boolean", exc);
        }

        // Unify the true option with the requested type ref
        try {
            trueOption.unify(typeRef, env);
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Can't unify true path in if expression", exc);
        }
        // Unify the false option with the requested type ref, which also has the effect of unifying
        // it with the true option
        try {
            falseOption.unify(typeRef, env);
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Can't unify false path in if expression", exc);
        }
    }

    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("(if ");
        builder.append(test.toString());
        builder.append(" ");
        builder.append(trueOption.toString());
        builder.append(" ");
        builder.append(falseOption.toString());
        builder.append(")");
        return builder.toString();
    }
}

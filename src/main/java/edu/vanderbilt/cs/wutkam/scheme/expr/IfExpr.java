package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.BooleanType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheme.type.UnifyException;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/24/21
 * Time: 4:35 PM
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
    public Expression evaluate(Environment<Expression> env) throws LispException {
        Expression testResult = test.evaluate(env);
        if (testResult instanceof BoolExpr) {
            if (((BoolExpr)testResult).value) {
                return trueOption.evaluate(env);
            } else {
                return falseOption.evaluate(env);
            }
        } else {
            throw new LispException("If expression test did not evaluate to a boolean");
        }
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        TypeRef testType = new TypeRef(BooleanType.TYPE);
        try {
            test.unify(testType, env);
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Test for if expression must be a boolean", exc);
        }
        try {
            trueOption.unify(typeRef, env);
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Can't unify true path in if expression", exc);
        }
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

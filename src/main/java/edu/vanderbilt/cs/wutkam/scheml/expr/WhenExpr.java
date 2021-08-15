package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.BooleanType;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheml.type.UnifyException;
import edu.vanderbilt.cs.wutkam.scheml.type.VoidType;

import java.util.ArrayList;
import java.util.List;

/** Represents a when expression that has only a true option. Since there is no false
 * option, this expression must have type void, and is typically used just for printing
 * results.
 * Since there is no need to distinguish between true and false expressions,
 * you can have a list of expressions.
 */
public class WhenExpr implements Expression {
    public final Expression test;
    public final List<Expression> trueOption;

    public WhenExpr(Expression test, List<Expression> trueOption)
    {
        this.test = test;
        this.trueOption = trueOption;
    }

    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        Expression testResult = test.evaluate(env, false);
        if (testResult instanceof BoolExpr) {
            // If the if expression is in the tail position then so are trueOption and falseOption
            if (((BoolExpr)testResult).value) {
                for (Expression expr: trueOption) {
                    expr.evaluate(env, inTailPosition);
                }
            }
            return new VoidExpr();
        } else {
            throw new LispException("When expression test did not evaluate to a boolean");
        }
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        // Unify the test with boolean
        TypeRef testType = new TypeRef(BooleanType.TYPE);
        try {
            test.unify(testType, env);
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Test for when expression must be a boolean", exc);
        }

        TypeRef voidType = new TypeRef(VoidType.TYPE);

        // Unify the true option with the requested type ref
        try {
            for (Expression expr: trueOption) {
                TypeRef dontCare = new TypeRef();
                // We unify each expression but don't care about the result
                expr.unify(dontCare, env);
            }
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Can't unify true path in when expression", exc);
        }

        try {
            typeRef.unify(voidType);
        } catch (UnifyException exc) {
            throw UnifyException.addCause("(when) form returns a void type", exc);
        }
    }

    @Override
    public Expression toScheml() {
        List<Expression> scheml = new ArrayList<>();
        scheml.add(new SymbolLiteralExpr("when"));
        scheml.add(test.toScheml());
        for (Expression expr: trueOption) {
            scheml.add(expr.toScheml());
        }
        return new ListExpr(scheml);
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("(when ");
        builder.append(test.toString());
        builder.append(" ");
        boolean isFirst = true;
        for (Expression expr: trueOption) {
            if (!isFirst) builder.append(' ');
            isFirst = false;
            builder.append(expr.toString());
        }
        builder.append(")");
        return builder.toString();
    }
}

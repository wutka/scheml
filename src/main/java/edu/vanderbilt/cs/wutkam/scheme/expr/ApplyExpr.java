package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;

import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/25/21
 * Time: 2:08 PM
 */
public class ApplyExpr implements Expression {
    public final Expression targetExpression;
    public final List<Expression> parameters;

    public ApplyExpr(Expression targetExpression, List<Expression> parameters) {
        this.targetExpression = targetExpression;
        this.parameters = parameters;
    }

    @Override
    public Expression evaluate(Environment<Expression> env) throws LispException {
        Expression maybeFunctionExpr = targetExpression.evaluate(env);

        if (!(maybeFunctionExpr instanceof FunctionExpr)) {
            throw new LispException("Expression "+maybeFunctionExpr.toString()+" is not a function");
        }
        FunctionExpr targetFunction = (FunctionExpr) targetExpression.evaluate(env);

        ArrayList<Expression> parameterList = new ArrayList<>();
        for (int i=0; i < parameters.size(); i++) {
            parameterList.add(parameters.get(i).evaluate(env));
        }
        return targetFunction.apply(parameterList, env);
    }
}

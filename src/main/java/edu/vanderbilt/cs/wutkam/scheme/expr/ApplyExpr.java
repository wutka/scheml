package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.FunctionType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheme.type.UnifyException;

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

    @Override
    public void unify(TypeRef ref, Environment<TypeRef> env) throws LispException {
        TypeRef targetType = new TypeRef();
        try {
            targetExpression.unify(targetType, env);
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Unable to unify application target", exc);
        }

        if (!(targetType.getType() instanceof FunctionType)) {
            throw new UnifyException("Target of function application "+targetType.getType()+" is not a function");
        }

        FunctionType targetFunc = (FunctionType) targetType.getType();

        if (parameters.size() > targetFunc.arity) {
            throw new UnifyException("Too many parameters in apply, expected "+targetFunc.arity+", got "+
                    parameters.size());
        }

        TypeRef[] unifiedParams = new TypeRef[parameters.size()];
        for (int i=0; i < parameters.size(); i++) {
            unifiedParams[i] = new TypeRef();
            parameters.get(i).unify(unifiedParams[i], env);
        }

        for (int i=0; i < parameters.size(); i++) {
            unifiedParams[i].unify(targetFunc.paramTypes[i]);
        }

        ref.unify(targetFunc.returnType);
    }
}

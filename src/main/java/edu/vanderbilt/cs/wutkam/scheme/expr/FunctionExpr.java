package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;

import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/25/21
 * Time: 2:07 PM
 */
public class FunctionExpr implements Expression {
    public final int arity;
    public final List<Expression> partialArgs;
    public final Expression targetExpression;
    public final List<SymbolExpr> parameterList;

    public FunctionExpr(int arity, List<SymbolExpr> parameterList, Expression targetExpression) {
        this.arity = arity;
        this.parameterList = parameterList;
        this.targetExpression = targetExpression;
        this.partialArgs = new ArrayList<>();
    }

    public FunctionExpr(FunctionExpr partialFunc, List<Expression> partialArgs) {
        this.arity = partialFunc.arity;
        this.parameterList = partialFunc.parameterList;
        this.targetExpression = partialFunc.targetExpression;
        this.partialArgs = new ArrayList<>();
        this.partialArgs.addAll(partialFunc.partialArgs);
        this.partialArgs.addAll(partialArgs);
    }

    @Override
    public Expression evaluate(Environment<Expression> env) throws LispException {
        return Expression.super.evaluate(env);
    }

    protected Expression apply(List<Expression> arguments, Environment<Expression> env)
        throws LispException {
        if (arguments.size() + partialArgs.size() > arity) {
            throw new LispException("Too many parameters passed to function "+this);
        }

        if (arguments.size() + partialArgs.size() == arity) {
            Environment<Expression> funcEnv = new Environment<>(env);
            for (int i=0; i < arity; i++) {
                if (i < partialArgs.size()) {
                    funcEnv.define(parameterList.get(i).value, partialArgs.get(i));
                } else {
                    funcEnv.define(parameterList.get(i).value, arguments.get(i-partialArgs.size()));
                }
            }
            return targetExpression.evaluate(funcEnv);
        } else {
            return new FunctionExpr(this, arguments);
        }
    }
}

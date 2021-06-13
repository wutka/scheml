package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;

import java.util.List;

public class ClosureExpr extends FunctionExpr {

    public ClosureExpr(String name, int arity, List<SymbolExpr> parameterList, List<Expression> targetExpressions) {
        super(name, arity, parameterList, targetExpressions);
    }

    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        // Make sure the closure only captures the environment in which it is first evaluated
        this.closureEnv = new Environment<>();
        this.closureEnv.copyCompressed(env);

        return new FunctionExpr(this);
    }
}

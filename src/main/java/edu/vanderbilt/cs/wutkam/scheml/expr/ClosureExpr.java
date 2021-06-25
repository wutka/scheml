package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;

import java.util.List;

/** A function declaration that captures its environment when it is evaluated and then returns a FunctionExpr
 * so that if it gets evaluated again, it doesn't capture its environment
 */
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

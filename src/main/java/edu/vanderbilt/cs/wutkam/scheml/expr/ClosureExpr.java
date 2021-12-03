package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;

import java.util.ArrayList;
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
        Environment<Expression> closureEnv = new Environment<>();
        closureEnv.copyCompressed(env);

        return new FunctionExpr(this, closureEnv);
    }

    @Override
    public Expression toScheml() {
        List<Expression> scheml = new ArrayList<>();
        scheml.add(new SymbolLiteralExpr("lambda"));
        List<Expression> params = new ArrayList<>();
        for (SymbolExpr paramSym: parameterList) {
            params.add(new SymbolLiteralExpr(paramSym.value));
        }
        scheml.add(new ListExpr(params));
        for (Expression expr: targetExpressions) {
            scheml.add(expr.toScheml());
        }
        return new ListExpr(scheml);
    }
}
